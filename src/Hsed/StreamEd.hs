-- |
-- Module      :  StreamEd
-- Copyright   :  (c) Vitaliy Rukavishnikov
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The Sed runtime engine

module Hsed.StreamEd where

import System.IO 
import Control.Monad (unless, when, forM_, zipWithM)
import qualified Control.Monad.State as S
import Control.Monad.Trans.Goto
import Data.List (isPrefixOf)
import Data.Char (isPrint)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as B
import Text.Printf (printf)

import Hsed.Parsec (parseSed, sedCmds)
import Hsed.Ast
import Hsed.SedRegex
import Hsed.SedState

type SedEngine a = GotoT a (S.StateT Env IO) a

data Status = EOF | Cont 
   deriving (Eq, Show)

data FlowControl = 
    Next                      -- ^ Apply the next sed command from the script to the pattern space
  | Break                     -- ^ Read the new line to the pattern space and apply sed script  
  | Continue                  -- ^ Reapply the sed script to the current pattern space
  | Goto (Maybe B.ByteString) -- ^ Jump to the marked sed command and apply it to the pattern space   
  | Exit                      -- ^ Quit 
   deriving (Eq, Show)

-- | Compile and execute the sed script
runSed :: [FilePath] -> String -> Env -> IO Env
runSed fs sed env = do
  S.execStateT (runGotoT $ do
       when ("#n" `isPrefixOf` sed) $ 
         S.lift $ set defOutput False 
       S.lift $ compile sed
       execute fs 
   ) env

-- | Parse the Sed commands
compile :: String -> SedState ()
compile cmds = do
  case parseSed sedCmds cmds of
     Right x -> set ast x
     Left e  -> error $ show e ++ " in " ++ cmds
  return ()

-- | Execute the parsed Sed commands against input data
execute :: [FilePath] -> SedEngine ()
execute fs = do
   processFiles fs
   fout <- S.lift $ get fileout
   S.liftIO $ S.mapM_ hClose (map snd fout)

-- | Process the input text files
processFiles :: [FilePath] -> SedEngine ()
processFiles files = do
   if null files then processFile stdin True
    else do
      let len = length files
      let fs = zipWith (\x y -> (x, y == len)) files [1..len]
      S.forM_ fs $ \(file, lastFile) -> do
         h <- S.liftIO $ openFile file ReadMode
         processFile h lastFile
   where
      processFile h lastFile = do
         S.lift $ set curFile (h, lastFile)
         nextLine

-- | Process the next input line from the file
nextLine :: SedEngine ()
nextLine = do
    (res, str) <- S.lift line
    case res of
        EOF  -> return ()
        Cont -> do
          S.lift $ set patternSpace str
          S.lift $ set appendSpace []
          cs <- S.lift $ get ast
          execCmds cs  
          nextLine

-- | Execute sed script
execCmds :: [SedCmd] -> SedEngine ()
execCmds cs = do
    forM_ cs $ \cmd -> do
      sch <- S.lift $ execCmd cmd
      case sch of
        Next -> return ()
        Break -> goto nextLine
        Continue -> goto (execCmds cs >> nextLine)
        Goto lbl -> (S.lift $ get ast) >>= \a -> goto (execCmds (jump a lbl) >> nextLine)
        Exit -> prnPat >> goto (return ())
    prnPat
    where prnPat = S.lift $ printPatSpace >> get appendSpace >>= \a -> mapM_ prnStr a

-- | Transfer control to the command marked with the label
jump :: [SedCmd] -> Maybe Label -> [SedCmd]      
jump cmds = maybe [] (go cmds)
  where 
        go [] _ = []
        go (SedCmd _ fun:cs) str = case fun of
           Group cs' -> go cs' str
           Label x -> if x == str then cs
                       else go cs str
           _ -> go cs str 

-- | Read an input line
line :: SedState (Status, B.ByteString)
line = do
   (h,b) <- get curFile
   p <- S.lift $ hIsEOF h
   if p then return (EOF,B.empty)
     else do 
       str <- S.lift $ B.hGetLine h
       modify curLine (+1)
       isLast <- if h == stdin then return False 
                   else S.lift (hIsEOF h) >>= \eof -> return eof
       if isLast && b then                
           get curLine >>= \l -> set lastLine l >> return (Cont, str)
        else return (Cont, str)  

-- | Execute the Sed function if the address is matched
execCmd :: SedCmd -> SedState FlowControl
execCmd (SedCmd a fun) = do
     b <- matchAddress a
     if b then runCmd fun
      else return Next

-- | Check if the address interval is matched  
matchAddress :: Address -> SedState Bool
matchAddress (Address addr1 addr2 invert) = 
    case (addr1,addr2) of
      (Just x, Nothing) -> matchAddr x x >>= \b -> return $ b /= invert
      (Just x, Just y)  -> matchAddr x y >>= \b -> return $ b /= invert
      _                 -> return $ not invert
    where
      matchAddr :: Addr -> Addr -> SedState Bool
      matchAddr a1 a2 = do 
         lineNum <- get curLine
         patSpace <- get patternSpace
         lastLineNum <- get lastLine 
         case (a1,a2) of
           (LineNumber x, LineNumber y) -> matchRange (x == lineNum) (y == lineNum)
           (LineNumber x, Pat y) -> matchRange (x == lineNum) (matchRE y patSpace)
           (LineNumber x, LastLine) -> matchRange (x == lineNum) (lineNum == lastLineNum)
           (LastLine, _) -> return $ lineNum == lastLineNum
           (Pat x, Pat y) -> matchRange (matchRE x patSpace) (matchRE y patSpace)
           (Pat x, LineNumber y) -> matchRange (matchRE x patSpace) (y == lineNum) 
           (Pat x, LastLine) -> matchRange (matchRE x patSpace) (lineNum == lastLineNum)       
      matchRange :: Bool -> Bool -> SedState Bool
      matchRange b1 b2 = do
         let setRange = set inRange
         range <- get inRange           
         if not range then 
            if b1 && b2 then return True
             else if b1 then setRange True >> return True
                   else return False
          else if b2 then setRange False >> return True
                else return True

-- | Execute the Sed function
runCmd :: SedFun -> SedState FlowControl
runCmd cmd = 
    case cmd of
      Group cs -> group cs
      LineNum -> lineNum
      Append txt -> append txt
      Branch lbl -> branch lbl
      Change txt -> change txt
      DeleteLine -> deleteLine
      DeletePat -> deletePat
      ReplacePat -> replacePat
      AppendPat -> appendPat
      ReplaceHold -> replaceHold
      AppendHold -> appendHold
      Insert txt -> insert txt
      List -> list
      NextLine -> next
      AppendLinePat -> appendLinePat
      PrintPat -> printPat
      WriteUpPat -> writeUpPat
      Quit -> quit
      ReadFile file -> readF file
      Substitute pat repl fs -> substitute pat repl fs
      Test lbl -> test lbl
      WriteFile file -> writeF file
      Exchange -> exchange
      Transform t1 t2 -> transform t1 t2
      Label lbl -> label lbl
      Comment -> comment
      EmptyCmd -> emptyCmd

-- | '{cmd...}' Groups subcommands enclosed in {} (braces)
group :: [SedCmd]  -> SedState FlowControl 
group [] = return Next
group (cmd:xs) = do
    sch <- execCmd cmd
    if sch == Next then
       group xs
     else return sch

-- | '=' Writes the current line number to standard output as a line
lineNum :: SedState FlowControl
lineNum = 
    get curLine >>= 
    (prnStrLn . B.pack . show) >> 
    return Next

-- | 'a\\ntext' Places the text variable in output before reading 
-- the next input line
append :: B.ByteString -> SedState FlowControl
append txt = 
    modify appendSpace (++ [txt,B.pack "\n"]) >> 
    return Next 

-- | 'b label' Transfer control to :label elsewhere in script
branch :: Maybe Label -> SedState FlowControl
branch = return . Goto

-- | 'c\\ntext' Replace the lines with the text variable
change :: B.ByteString -> SedState FlowControl
change txt = do
    range <- get inRange
    unless range $ prnStrLn txt
    return Break

-- | 'd' Delete line(s) from pattern space
deleteLine :: SedState FlowControl
deleteLine = 
    set patternSpace B.empty >> 
    return Break

-- | 'D' Delete first part (up to embedded newline) of multiline pattern space
deletePat :: SedState FlowControl
deletePat = do
    p <- get patternSpace
    let p' = B.drop 1 $ B.dropWhile (/='\n') p
    set patternSpace p'
    return Continue

-- | 'g' Copy contents of hold space into the pattern space
replacePat :: SedState FlowControl
replacePat = 
    get holdSpace >>= \h -> 
    set patternSpace h >> 
    return Next

-- | 'G' Append newline followed by contents of hold space 
-- to contents of the pattern space.
appendPat :: SedState FlowControl
appendPat = 
    get holdSpace >>= \h -> 
    modify patternSpace (`B.append` B.cons '\n' h) >> 
    return Next

-- | 'h' Copy pattern space into hold space
replaceHold :: SedState FlowControl
replaceHold = 
    get patternSpace >>= \p -> 
    set holdSpace p >> 
    return Next

-- | 'H' Append newline and contents of pattern space to contents 
-- of the hold space
appendHold :: SedState FlowControl
appendHold = 
    get patternSpace >>= \p -> 
    modify holdSpace (`B.append` B.cons '\n' p) >> 
    return Next

-- | 'i\\ntext' Writes the text variable to standard output before 
-- reading the next line into the pattern space.
insert :: B.ByteString -> SedState FlowControl
insert txt = prnStrLn txt >> return Next

-- | 't label' Jump to line if successful substitutions have been made
test :: Maybe Label -> SedState FlowControl
test lbl = 
    get subst >>= \s -> 
    if s then return $ Goto lbl 
     else return Next

-- | 's/pattern/replacement/[flags]' Substitute replacement for pattern
substitute :: B.ByteString -> B.ByteString -> Flags -> SedState FlowControl
substitute pat repl fs = do
  let (gn, p, w) = getFlags fs 
  patSpace <- get patternSpace
  let (repl', b) = sedSubRegex pat patSpace repl gn
  set subst b
  when b $ do
    set patternSpace repl'
    when p $ get patternSpace >>= \ps -> prnStrLn ps
    unless (null w) $ writeF w >> return ()
  return Next
   where
     getFlags :: Flags -> (Int, Bool, FilePath)
     getFlags (Flags o f) = (occurr, printPat, file) where
        (occurr, printPat) = case o of
           Nothing -> (1, False)
           Just (OccurrencePrint x y) -> (occ x, y)
           Just (PrintOccurrence x y) -> (occ y, x)
        occ x = case x of
           Nothing -> 1
           Just ReplaceAll -> 0
           Just (Replace n) -> n        
        file = fromMaybe "" f

-- | 'n' Read next line of input into pattern space. 
next :: SedState FlowControl
next = do
    printPatSpace
    (res,str) <- line
    set patternSpace str
    if res == EOF then return Break else return Next

-- | 'l' List the contents of the pattern space, showing 
-- nonprinting characters as ASCII codes
list :: SedState FlowControl
list = do
    patSpace <- get patternSpace
    S.forM_ (B.unpack patSpace) $ \ch ->
      if isPrint ch then prnChar ch
       else case lookup ch esc of
             Nothing -> do 
                 prnChar '\\'
                 prnPrintf ch
             Just x -> prnStr (B.pack x)
    prnChar '\n'
    return Next
    where esc = zip "\\\a\b\f\r\t\v"
                    ["\\\\","\\a", "\\b", "\\f", "\\r", "\\t", "\\v"]

-- | 'x' Exchange contents of the pattern space with the 
-- contents of the hold space
exchange :: SedState FlowControl
exchange = do
    hold <- get holdSpace
    pat  <- get patternSpace
    set holdSpace pat
    set patternSpace hold
    return Next

-- | 'N' Append next input line to contents of pattern space
appendLinePat :: SedState FlowControl
appendLinePat = do
    (res,ln) <- line
    if res == EOF then return Break
      else do
       let suffix = B.append (B.pack "\n") ln
       modify patternSpace (`B.append` suffix)
       return Next

-- | 'p' Print the lines
printPat :: SedState FlowControl
printPat = 
     get patternSpace >>= \p -> 
     prnStrLn p >> 
     return Next

-- | 'P' Print first part (up to embedded newline) of 
-- multiline pattern space
writeUpPat :: SedState FlowControl
writeUpPat = 
     get patternSpace >>= 
     (prnStrLn . B.takeWhile (/='\n')) >> 
     return Next

-- | 'q' Quit
quit :: SedState FlowControl
quit = return Exit

-- | 'y/abc/xyz' Transform each character by position in string abc 
-- to its equivalent in string xyz
transform :: B.ByteString -> B.ByteString -> SedState FlowControl
transform t1 t2 = do
    when (B.length t1 /= B.length t2) $ 
      error "Transform strings are not the same length"
    patSpace <- get patternSpace
    let tr = B.map go patSpace
    set patternSpace tr 
    return Next
    where go ch = fromMaybe ch (lookup ch (B.zip t1 t2))

-- | 'w file' Append contents of pattern space to file
writeF :: FilePath -> SedState FlowControl
writeF file = do
    fout <- get fileout
    patSpace <- get patternSpace
    let printFileout h = S.lift $ B.hPutStrLn h patSpace
    case lookup file fout of
       Nothing -> do
          h <- S.lift $ openFile file WriteMode
          modify fileout (++ [(file,h)])
          printFileout h
       Just h -> printFileout h
    return Next

-- | 'r' Read contents of file and append after the contents of the 
-- pattern space
readF :: FilePath -> SedState FlowControl
readF file = do
    cont <- S.lift $ B.readFile file `catch` \_ -> return B.empty
    modify appendSpace (++ [cont])
    return Next

-- | Skip label, comment and empty command
label _ = return Next
comment  = return Next
emptyCmd = return Next

-- | Print the pattern space to the standard output
printPatSpace :: SedState ()
printPatSpace = do
   out <- get defOutput
   when out $ get patternSpace >>= \p -> prnStrLn p

-- | Check if the current line in the pattern space is the last line
isLastLine :: SedState Bool
isLastLine = do
    l <- get lastLine
    cur <- get curLine
    return $ l == cur

-- | Writes the string to the standard output or save the string in the memory buffer
prnStr :: B.ByteString -> SedState ()
prnStr str = do
   useMem <- get useMemSpace
   if useMem then modify memorySpace (`B.append` str) 
     else S.lift $ B.putStr str

-- | The same as prnStr, but adds a newline character
prnStrLn :: B.ByteString -> SedState ()
prnStrLn str = prnStr $ B.snoc str '\n'

-- | The same as prnStr, but for char
prnChar :: Char -> SedState ()
prnChar c = prnStr $ B.singleton c

-- | Print the character as three-digit octal number
prnPrintf :: Char -> SedState ()
prnPrintf c = do
    let str = printf "%03o" c :: String 
    prnStr $ B.pack str
