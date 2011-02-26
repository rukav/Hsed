{-# LANGUAGE DeriveDataTypeable #-}

import System.Directory (getDirectoryContents)
import System.FilePath (replaceExtension, takeExtension, (</>))
import Control.Monad (forM_, when)
import qualified Control.Exception as E
import qualified Control.Monad.State as S
import System.Console.CmdArgs
import Data.List (isPrefixOf)
import qualified Data.ByteString.Char8 as B
import Parsec (parseSed, sedCmds)
import StreamEd
import SedState
import Sed (execScript')

data Config = Config {
  dir :: FilePath,
  inp :: String,
  ok :: String,
  file :: FilePath
} deriving (Show, Data, Typeable)

config :: Mode (CmdArgs Config)
config = cmdArgsMode $ Config
   {dir = def &= typFile &= help "Directory with the test cases"
   ,inp = def &= help "Input file extension"
   ,ok  = def &= help "Output file extension"
   ,file= def &= typFile &= help "Sed script to test"
   } &=
   program "TestSuite" &=
   summary "TestSuite 0.1" &=
   help "" &=
   details []

data Result = OK | Diff String | Error String 
  deriving (Show, Eq, Typeable)

main :: IO ()
main = do
  cnf <- cmdArgsRun config
  let script = file cnf
  testParse
  if (not.null) script then
    testFile script (inp cnf) (ok cnf)
   else
    testDir (dir cnf) (inp cnf) (ok cnf) 

passed :: String -> String
passed script = result script " is passed"

failed :: String -> String
failed script = result script " is failed"

result :: String -> String -> String
result script str = "Test " ++ script ++ str

testParse :: IO ()
testParse = do
   putStrLn "<-- Sed parser tests started"
   forM_ ptests $ \(str, etalon) -> 
      case parseSed sedCmds str of
        Left x -> putStrLn  (failed str ++ " -> ") >> print x
        Right y -> if show y == etalon then putStrLn $ passed (show str)
                    else do 
                     putStrLn $ failed str
                     print etalon
                     print "====>"
                     print y

   putStrLn "--> Sed parser tests ended\n"
   
testFile :: FilePath -> String -> String -> IO ()
testFile script inext okext = do
    let inpfile = replaceExtension script inext
    let okfile = replaceExtension script okext    
    sed <- readFile script `catch` openFileError script
    res <- execScript' [inpfile] sed
    okf <- B.readFile okfile `catch` openFileErrorB okfile 
    if res == okf then 
      putStrLn $ passed script
     else do
      putStrLn $ failed script  
      mapM_ print (B.lines okf)
      print "===>"
      mapM_ print (B.lines res)
    where
      openFileErrorB f e = putStr ("Error: Couldn't open " ++ f ++ 
           ": " ++ show (e :: E.IOException)) >> return B.empty

testDir :: FilePath -> String -> String -> IO ()
testDir path inpext okext = do
    putStrLn "<-- Sed script tests started"
    names <- getDirectoryContents path
    forM_ names $ \n -> do
       let filePath = path </> n
       when (takeExtension filePath == ".sed") $
           testFile filePath inpext okext
    putStrLn "--> Sed script tests ended"

ptests :: [(String, String)]
ptests = [
 ("/123/=",
  "[SedCmd (Address (Just (Pat \"123\")) Nothing False) LineNum]"),
 ("/123/{\n=\n}",
  "[SedCmd (Address (Just (Pat \"123\")) Nothing False) (Group [SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) LineNum])]"),
 ("a\\\n123\\\n456",
  "[SedCmd (Address Nothing Nothing False) (Append \"123\\n456\")]"),
 ("a\\\n~",
  "[SedCmd (Address Nothing Nothing False) (Append \"~\")]"),
 ("/123/,/456/b", 
  "[SedCmd (Address (Just (Pat \"123\")) (Just (Pat \"456\")) False) (Branch Nothing)]"),
 ("y/aaa/bbb/",
  "[SedCmd (Address Nothing Nothing False) (Transform \"aaa\" \"bbb\")]"),
 ("s/aaa/bbb/g",
  "[SedCmd (Address Nothing Nothing False) (Substitute \"aaa\" \"bbb\" (Flags (Just (OccurrencePrint (Just ReplaceAll) False)) Nothing))]"),
 ("s'aaa'b&bb'1pw 123",
  "[SedCmd (Address Nothing Nothing False) (Substitute \"aaa\" \"b\\\\0bb\" (Flags (Just (OccurrencePrint (Just (Replace 1)) True)) (Just \"123\")))]"),
 ("s/aaa/aa\1bb/g",
  "[SedCmd (Address Nothing Nothing False) (Substitute \"aaa\" \"aa\\SOHbb\" (Flags (Just (OccurrencePrint (Just ReplaceAll) False)) Nothing))]"),
 ("s/aaa/\3/",
  "[SedCmd (Address Nothing Nothing False) (Substitute \"aaa\" \"\\ETX\" (Flags (Just (OccurrencePrint Nothing False)) Nothing))]"),
 ("r ReadFile.cmd",
  "[SedCmd (Address Nothing Nothing False) (ReadFile \"ReadFile.cmd\")]"),
 ("w WriteFile.cmd",
  "[SedCmd (Address Nothing Nothing False) (WriteFile \"WriteFile.cmd\")]"),
 ("{\n=\n=\n}",
  "[SedCmd (Address Nothing Nothing False) (Group [SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) LineNum,SedCmd (Address Nothing Nothing False) LineNum])]"),
 ("=\n=\n",
  "[SedCmd (Address Nothing Nothing False) LineNum,SedCmd (Address Nothing Nothing False) LineNum]"),
 (" #=",
  "[SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) Comment]"),
 ("#n Print line before and after changes.\n{\n=\n}",
  "[SedCmd (Address Nothing Nothing False) Comment,SedCmd (Address Nothing Nothing False) (Group [SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) LineNum])]"),
 ("r ReadFile.cmd\nr ReadFile.cmd",
  "[SedCmd (Address Nothing Nothing False) (ReadFile \"ReadFile.cmd\"),SedCmd (Address Nothing Nothing False) (ReadFile \"ReadFile.cmd\")]"),
 ("/^\\.H1/n\n/^$/d",
  "[SedCmd (Address (Just (Pat \"^\\\\.H1\")) Nothing False) NextLine,SedCmd (Address (Just (Pat \"^$\")) Nothing False) DeleteLine]"),
 ("s/      /\\n/2",
  "[SedCmd (Address Nothing Nothing False) (Substitute \"      \" \"\\n\" (Flags (Just (OccurrencePrint (Just (Replace 2)) False)) Nothing))]"),
 ("s/Owner and Operator\\nGuide/aaa/g",
  "[SedCmd (Address Nothing Nothing False) (Substitute \"Owner and Operator\\nGuide\" \"aaa\" (Flags (Just (OccurrencePrint (Just ReplaceAll) False)) Nothing))]"),
 ("/Operator$/{\nN\ns/Owner and Operator\\nGuide/Installation Guide/\n}\n",
  "[SedCmd (Address (Just (Pat \"Operator$\")) Nothing False) (Group [SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) AppendLinePat,SedCmd (Address Nothing Nothing False) (Substitute \"Owner and Operator\\nGuide\" \"Installation Guide\" (Flags (Just (OccurrencePrint Nothing False)) Nothing))])]"),
 ("s/Owner and Operator Guide/Installation Guide/\n/Owner/{\nN\ns/ *\n/ /\ns/Owner andOperator Guide */Installation Guide\\n/\n}",
  "[SedCmd (Address Nothing Nothing False) (Substitute \"Owner and Operator Guide\" \"Installation Guide\" (Flags (Just (OccurrencePrint Nothing False)) Nothing)),SedCmd (Address (Just (Pat \"Owner\")) Nothing False) (Group [SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) AppendLinePat,SedCmd (Address Nothing Nothing False) (Substitute \" *\\n\" \" \" (Flags (Just (OccurrencePrint Nothing False)) Nothing)),SedCmd (Address Nothing Nothing False) (Substitute \"Owner andOperator Guide *\" \"Installation Guide\\n\" (Flags (Just (OccurrencePrint Nothing False)) Nothing))])]"),
 ("    /^\n$/D",
  "[SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address (Just (Pat \"^\\n$\")) Nothing False) DeletePat]"),
 ("{\n{\n}\n}",
  "[SedCmd (Address Nothing Nothing False) (Group [SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) (Group [SedCmd (Address Nothing Nothing False) EmptyCmd])])]"),
 ("{\n{\nN\n}\n}",
  "[SedCmd (Address Nothing Nothing False) (Group [SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) (Group [SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) AppendLinePat])])]"),
 ("/UNIX/{\n  N\n  /\nSystem/{\n  s// Operating &/\n  P\n  D\n  }\n}",
  "[SedCmd (Address (Just (Pat \"UNIX\")) Nothing False) (Group [SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) AppendLinePat,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address (Just (Pat \"\\nSystem\")) Nothing False) (Group [SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) (Substitute \"\\nSystem\" \" Operating \\\\0\" (Flags (Just (OccurrencePrint Nothing False)) Nothing)),SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) WriteUpPat,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) DeletePat,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd])])]"),
 ("=\n=\n  #\n",
  "[SedCmd (Address Nothing Nothing False) LineNum,SedCmd (Address Nothing Nothing False) LineNum,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) Comment]"),
 ("  \n =\n \n =\n",
  "[SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) LineNum,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) LineNum]"),
 ("  \n \n \n \n",
  "[SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd]"),
 ("{N\nN\n{=\n  }\n}",
  "[SedCmd (Address Nothing Nothing False) (Group [SedCmd (Address Nothing Nothing False) AppendLinePat,SedCmd (Address Nothing Nothing False) AppendLinePat,SedCmd (Address Nothing Nothing False) (Group [SedCmd (Address Nothing Nothing False) LineNum,SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd])])]"),
 ("{\n }",
  "[SedCmd (Address Nothing Nothing False) (Group [SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) EmptyCmd])]"),
 ("{\nN\n}\n#/Owner/{}",
  "[SedCmd (Address Nothing Nothing False) (Group [SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) AppendLinePat]),SedCmd (Address Nothing Nothing False) Comment]"),
 ("{\n}\nP",
  "[SedCmd (Address Nothing Nothing False) (Group [SedCmd (Address Nothing Nothing False) EmptyCmd]),SedCmd (Address Nothing Nothing False) WriteUpPat]"),
 ("N\nP",
  "[SedCmd (Address Nothing Nothing False) AppendLinePat,SedCmd (Address Nothing Nothing False) WriteUpPat]"),
 ("1!H;1!{ x; p; x \n};d",
  "[SedCmd (Address (Just (LineNumber 1)) Nothing True) AppendHold,SedCmd (Address (Just (LineNumber 1)) Nothing True) (Group [SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) Exchange,SedCmd (Address Nothing Nothing False) PrintPat,SedCmd (Address Nothing Nothing False) Exchange]),SedCmd (Address Nothing Nothing False) DeleteLine]"),
 ("/^0$/{\nc\\\nyes\n}",
  "[SedCmd (Address (Just (Pat \"^0$\")) Nothing False) (Group [SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) (Change \"yes\")])]"),
 ("2,5 {\ns/[\t ]//g\n}",
  "[SedCmd (Address (Just (LineNumber 2)) (Just (LineNumber 5)) False) (Group [SedCmd (Address Nothing Nothing False) EmptyCmd,SedCmd (Address Nothing Nothing False) (Substitute \"[\\t ]\" \"\" (Flags (Just (OccurrencePrint (Just ReplaceAll) False)) Nothing))])]")
--}
 ]
