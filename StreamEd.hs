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
import Data.Typeable
import Data.Char (isPrint)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as B
import Text.Printf (printf)
import Parsec (parseSed, sedCmds)
import Ast
import SedRegex
import SedState

data FileStatus = EOF | Cont 
   deriving (Eq, Show)

data CmdSchedule = None | NextCmd | Break | Continue | Jump (Maybe B.ByteString) | Exit
   deriving (Eq, Show)

-- | Run Sed program
run :: [String] -> IO ()
run args = do 
    S.execStateT (do 
           (files, cmds) <- parseArgs args
           compile cmds
           execute files
         ) initEnv
    return ()

-- | Parse Sed program's arguments
parseArgs :: [String] -> SedState ([FilePath], String)
parseArgs xs = parseArgs' xs ([],[]) where
    parseArgs' [] fs = return fs
    parseArgs' [x] fs 
       | x == "-n" = set defOutput False >> return fs
       | otherwise = parseCmds x fs
    parseArgs' (x:y:xs) fs
       | x == "-e" = parseArgs' xs (addCmd fs y)
       | x == "-f" = do
            sed <- S.lift $ System.IO.readFile y `catch` openFileError y
            when ("#n" `isPrefixOf` sed) $ set defOutput False 
            parseArgs' xs (addCmd fs sed)
       | x == "-n" = set defOutput False >> parseArgs' (y:xs) fs
       | otherwise = parseCmds x fs >>= \fs' -> parseArgs' (y:xs) fs'
       where
         addCmd (files, cmds) x = (files, cmds ++ ('\n':x))

openFileError f e = putStr ("Error: Couldn't open " ++ f ++ ": " ++ 
                    show (e :: E.IOException)) >> return []

-- | Parse Sed program's embedded commands and the input files arguments
parseCmds :: String -> ([FilePath], String) -> SedState ([FilePath], String)
parseCmds x (files, cmds) = 
    if null cmds then return (files, x) 
     else addFiles x (files,cmds)
    where
     addFiles :: String -> ([FilePath], String) -> SedState ([FilePath], String)
     addFiles x (files, cmds) = do
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
execute cont = 
   if null cont then processStdin
    else do 
     let (files,len) = (cont, length cont)
     let fs = zipWith (\x y -> (x, y == len)) files [1..len]
     processFiles fs
     fout <- get fileout
     S.lift $ S.mapM_ hClose (map snd fout)
     return ()

-- | The standard input will be used if no file operands are specified
processStdin :: SedState ()
processStdin = do
    e <- get exit
    unless e $ do
      a <- get ast
      loop stdin True a

-- | Process the input text files
processFiles :: [(FilePath, Bool)] -> SedState ()
processFiles fs = 
    S.forM_ fs $ \(file, lastFile) -> do
        e <- get exit
        unless e $ do
           h <- S.lift $ openFile file ReadMode
           a <- get ast
           loop h lastFile a

-- | Cyclically append a line of input without newline into the pattern space
loop :: Handle -> Bool -> [SedCmd] -> SedState ()
loop h b cs = do
      (res, str) <- line h b   
      case res of
        EOF -> S.lift $ hClose h >> return ()
        _   -> do
          set patternSpace str
          set appendSpace []
          sch <- eval h b cs
          case sch of 
            Exit -> return ()
            _    -> loop h b cs

-- | Read an input line and handle the EOF status
line :: Handle -> Bool -> SedState (FileStatus, B.ByteString)
line h b = do
   p <- S.lift $ hIsEOF h
   if p then return (EOF,B.empty)
     else do 
       line <- S.lift $ B.hGetLine h
       modify curLine (+1)
       isLast <- if h == stdin then return False 
                   else S.lift (hIsEOF h) >>= \b -> return b

       if isLast && b then                
           get curLine >>= \l -> set lastLine l >> return (Cont, line)
        else return (Cont, line)

-- | Manage the flow control of the Sed commands
eval :: Handle -> Bool -> [SedCmd] -> SedState CmdSchedule
eval h b cs = do
    sch <- execCmds cs (line h b)
    case sch of
       Jump lbl -> get ast >>= \as -> eval h b (goto as lbl)
       Exit -> printPatSpace >> get appendSpace >>= \a -> 
               mapM_ prnStr a >> set exit True >> return Exit
       _ -> get appendSpace >>= \a -> mapM_ prnStr a >> return sch

-- | Transfer control to the command marked with the label      
goto cmds = maybe [] (go cmds)
  where 
        go [] str = []
        go (SedCmd a fun:cs) str = case fun of
           Group cs' -> go cs' str
           Label x -> if x == str then cs
                       else go cs str
           _ -> go cs str   

-- | Execute the specified Sed commands
execCmds :: [SedCmd] -> SedState (FileStatus,B.ByteString) -> SedState CmdSchedule
execCmds [] _ = printPatSpace >> return None
execCmds (x:xs) line = do
    sch <- execCmd x line
    if sch `elem` [NextCmd, None] then execCmds xs line
     else if sch == Continue then do
       cs <- get ast
       execCmds cs line
     else return sch

-- | Execute the Sed command if the address interval of the command is matched
execCmd (SedCmd a fun) line = do
     b <- matchAddress a
     if b then runCmd fun line >>= \sch -> return sch
      else return NextCmd

-- | Check if the address interval is matched  
matchAddress :: Address -> SedState Bool
matchAddress (Address a1 a2 invert) = 
    case (a1,a2) of
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
runCmd :: SedFun -> SedState (FileStatus,B.ByteString) -> SedState CmdSchedule
runCmd cmd line = 
    case cmd of
      Group cs -> group cs line
      LineNum -> get curLine >>= (prnStrLn . B.pack . show) >> return NextCmd
      Append txt -> modify appendSpace (++ [txt]) >> return NextCmd
      Branch lbl -> return (Jump lbl)
      Change txt -> change txt
      Delete -> set patternSpace B.empty >> return Break
      DeletePat -> deletePat
      ReplacePat -> get holdSpace >>= \h -> set patternSpace h >> return NextCmd
      AppendPat -> get holdSpace >>= \h -> modify patternSpace (`B.append` B.cons '\n' h) 
                   >> return NextCmd
      ReplaceHold -> get patternSpace >>= \p -> set holdSpace p >> return NextCmd
      AppendHold -> get patternSpace >>= \p -> modify holdSpace (`B.append` B.cons '\n' p) 
                    >> return NextCmd
      Insert txt -> prnStrLn txt >> return NextCmd
      List -> list
      Next -> next line
      AppendLinePat -> appendLinePat line
      PrintPat -> get patternSpace >>= \p -> prnStrLn p >> return NextCmd
      WriteUpPat -> get patternSpace >>= (prnStrLn . B.takeWhile (/='\n')) >> return NextCmd
      Quit -> return Exit
      ReadFile file -> readF file
      Substitute pat repl fs -> substitute pat repl fs
      Test lbl -> get subst >>= \s -> if s then return $ Jump lbl else return NextCmd
      WriteFile file -> writeF file
      Exchange -> exchange
      Transform t1 t2 -> transform t1 t2
      Label str -> return NextCmd
      Comment -> return NextCmd
      EmptyCmd -> return None

-- | Execute 'group' Sed function    
group [] _ = return NextCmd
group (cmd:xs) line = do
    sch <- execCmd cmd line
    if sch `elem` [NextCmd, None] then
       group xs line
     else return sch

-- | Execute 'delete' from multiline pattern space Sed function
deletePat = do
    p <- get patternSpace
    set patternSpace (B.drop 1 $ B.dropWhile (/='\n') p)
    return Continue

-- | Execute 'substitute' Sed function
substitute pat repl fs = do
  let (gn, p, w) = getFlags fs 
  patSpace <- get patternSpace
  let (repl', b) = sedSubRegex pat patSpace repl gn
  set subst b
  if b then do
     set patternSpace repl'
     if p then get patternSpace >>= \p -> prnStrLn p >> return NextCmd 
       else return NextCmd
     if (not.null) w then writeF w >> return NextCmd else return NextCmd
   else return NextCmd
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

-- | Execute 'change' Sed function
change txt = do
    range <- get inRange
    if not range then do
       prnStrLn txt
       return Break
     else return Break

-- | Execute 'next' Sed function
next line = do
    printPatSpace
    (res,str) <- line
    set patternSpace str
    if res == EOF then return Break else return NextCmd

-- | Execute 'list' Sed function
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
    return NextCmd
    where esc = zip "\\\a\b\f\r\t\v"
                    ["\\\\","\\a", "\\b", "\\f", "\\r", "\\t", "\\v"]

-- | Execute 'exchange' Sed function
exchange = do
    hold <- get holdSpace
    pat  <- get patternSpace
    set holdSpace pat
    set patternSpace hold
    return NextCmd

-- | Execute 'appendLinePat' Sed function
appendLinePat line = do
    (res,ln) <- line
    if res == EOF then return Break
      else do
       let suffix = B.append (B.pack "\n") ln
       modify patternSpace (`B.append` suffix)
       return None

-- | Execute 'transform' Sed function
transform t1 t2 = do
    when (B.length t1 /= B.length t2) $ 
      error "Transform strings are not the same length"
    patSpace <- get patternSpace
    let tr = B.map go patSpace
    set patternSpace tr 
    return NextCmd
    where go ch = fromMaybe ch (lookup ch (B.zip t1 t2))

-- | Write contents of the pattern space to the file
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
    return NextCmd

-- | Add contents of the input file to the append space
readF file = do
    cont <- S.lift $ B.readFile file `catch` \_ -> return B.empty
    modify appendSpace (++ [cont])
    return NextCmd

-- | Print the pattern space to the standard output
printPatSpace :: SedState ()
printPatSpace = do
   out <- get defOutput
   when out $ get patternSpace >>= \p -> prnStrLn p

-- | Check if the current line in the pattern space is the last line
isLastLine = do
    last <- get lastLine
    cur <- get curLine
    return $ last == cur

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
