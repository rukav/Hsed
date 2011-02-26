{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      :  StreamEd
-- Copyright   :  (c) Vitaliy Rkavishnikov
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The Sed runtime engine. The execution sequence includes the parseArgs (parse the Sed options/args),
-- compile (parse) Sed commands, execute Sed commands.

module StreamEd where

import System.IO 
import System.FilePath (splitFileName)
import Control.Monad (unless, when)
import qualified Control.Monad.State as S
import qualified Control.Exception as E
import qualified System.FilePath.Glob as G (compile, globDir1) 
import Data.List (isPrefixOf)
import Data.Char (isPrint)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as B
import Text.Printf (printf)
import Parsec (parseSed, sedCmds)
import Ast
import SedRegex
import SedState

data Status = EOF | Cont 
   deriving (Eq, Show)

data FlowControl = 
    Next                      -- ^ Apply the next sed command from the script to the pattern space
  | Break                     -- ^ Read the new line to the pattern space and apply sed script  
  | Continue                  -- ^ Reapply the sed script to the current pattern space
  | Goto (Maybe B.ByteString) -- ^ Jump to the marked sed command and apply it to the pattern space   
  | Exit                      -- ^ Quit 
   deriving (Eq, Show)

-- | Run Sed program
run :: [String] -> IO ()
run args = 
    S.execStateT (do 
           (files, cmds) <- parseArgs args
           compile cmds
           execute files
         ) initEnv >> return ()

-- | Parse Sed program's arguments
parseArgs :: [String] -> SedState ([FilePath], String)
parseArgs xs = parseArgs' xs ([],[]) where
    parseArgs' [] fs = return fs
    parseArgs' [x] fs 
       | x == "-n" = set defOutput False >> return fs
       | otherwise = parseCmds x fs
    parseArgs' (x:y:ys) fs
       | x == "-e" = parseArgs' ys (addCmd fs y)
       | x == "-f" = do
            sed <- S.lift $ System.IO.readFile y `catch` openFileError y
            when ("#n" `isPrefixOf` sed) $ set defOutput False 
            parseArgs' ys (addCmd fs sed)
       | x == "-n" = set defOutput False >> parseArgs' (y:ys) fs
       | otherwise = parseCmds x fs >>= \fs' -> parseArgs' (y:ys) fs'
       where
         addCmd (files, cmds) x' = (files, cmds ++ ('\n':x'))

openFileError :: String -> E.IOException -> IO [a]
openFileError f e = putStr ("Error: Couldn't open " ++ f ++ ": " ++ 
                    show (e :: E.IOException)) >> return []

-- | Parse Sed program's embedded commands and the input files arguments
parseCmds :: String -> ([FilePath], String) -> SedState ([FilePath], String)
parseCmds x (files, cmds) = 
    if null cmds then return (files, x) 
     else do 
       let (dir, fp) = splitFileName x
       fs <- S.lift $ G.globDir1 (G.compile fp) dir
       if null fs then error $ fp ++ ": No such file or directory"
        else return (files ++ fs, cmds)

-- | Parse the Sed commands
compile :: String -> SedState ()
compile cmds = do
  case parseSed sedCmds cmds of
     Right x -> set ast x
     Left e  -> error $ show e ++ " in " ++ cmds
  return ()

-- | Execute the parsed Sed commands against input data
execute :: [FilePath] -> SedState ()
execute fs = do
   if null fs then processStdin
     else processFiles fs
   fout <- get fileout
   S.lift $ S.mapM_ hClose (map snd fout)

-- | The standard input will be used if no file operands are specified
processStdin :: SedState ()
processStdin = do
    e <- get exit
    unless e $ do
      set curFile (stdin, True)
      a <- get ast
      loop a

-- | Process the input text files
processFiles :: [FilePath] -> SedState ()
processFiles files = do
    let len = length files
    let fs = zipWith (\x y -> (x, y == len)) files [1..len]
    S.forM_ fs $ \(file, lastFile) -> do
        e <- get exit
        unless e $ do
           h <- S.lift $ openFile file ReadMode
           set curFile (h, lastFile)
           a <- get ast
           loop a

-- | Cyclically append a line of input without newline into the pattern space
loop :: [SedCmd] -> SedState ()
loop cs = do
      (res, str) <- line 
      case res of
        EOF -> get curFile >>= \(h,_) -> S.lift $ hClose h >> return ()
        _   -> do
          set patternSpace str
          set appendSpace []
          sch <- eval cs
          case sch of 
            Exit -> return ()
            _    -> loop cs

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

-- | Manage the flow control of the Sed commands
eval :: [SedCmd] -> SedState FlowControl
eval cs = do
    sch <- execCmds cs
    case sch of
       Goto lbl -> get ast >>= \as -> eval (goto as lbl)
       Exit -> printPatSpace >> get appendSpace >>= \a -> 
               mapM_ prnStr a >> set exit True >> return Exit
       _ -> get appendSpace >>= \a -> mapM_ prnStr a >> return sch

-- | Transfer control to the command marked with the label
goto :: [SedCmd] -> Maybe Label -> [SedCmd]      
goto cmds = maybe [] (go cmds)
  where 
        go [] _ = []
        go (SedCmd _ fun:cs) str = case fun of
           Group cs' -> go cs' str
           Label x -> if x == str then cs
                       else go cs str
           _ -> go cs str   

-- | Execute Sed commands
execCmds :: [SedCmd] -> SedState FlowControl
execCmds [] = printPatSpace >> return Next
execCmds (x:xs) = do
    sch <- execCmd x
    if sch `elem` [Next] then execCmds xs
     else if sch == Continue then do
       cs <- get ast
       execCmds cs
     else return sch

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
    modify appendSpace (++ [txt]) >> 
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
