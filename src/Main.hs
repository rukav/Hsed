-- |
-- Module      :  Main
-- Copyright   :  (c) Vitaliy Rukavishnikov
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- See "The Open Group Base Specifications Issue 7" for the requirements
-- This version of the Haskell Sed uses regex-posix package to parse all regular 
-- expressions. At the moment it doesn't supports the back-references in the RE.

module Main where

import System.IO
import System (getArgs)
import System.FilePath (splitFileName)
import Control.Monad (unless, when)
import qualified Control.Exception as E
import qualified System.FilePath.Glob as G (compile, globDir1) 

import Hsed.StreamEd (runSed)
import Hsed.SedState 

data SedArgs = SedArgs {
  files :: [FilePath],
  script :: String,
  defOut :: Maybe Bool
} deriving Show


main :: IO ()
main = do
    args <- getArgs
    if null args || head args == "--help" then do
      putStrLn usage
      return ()
     else do
       SedArgs fs sed out <- parseArgs args
       let setDef b = initEnv {defOutput_ = b}
       runSed fs sed (maybe initEnv setDef out)
       return ()

-- | Parse Sed program's arguments
parseArgs :: [String] -> IO SedArgs
parseArgs xs = parseArgs' xs (SedArgs [] "" Nothing) where
    parseArgs' [] sargs = return sargs
    parseArgs' [x] sargs
       | x == "-n" = return $ sargs {defOut = Just False}
       | otherwise = parseCmds x sargs
    parseArgs' (x:y:ys) sargs
       | x == "-e" = parseArgs' ys (addCmd sargs y)
       | x == "-f" = do
            sed <- System.IO.readFile y `catch` openFileError y
            parseArgs' ys (addCmd sargs sed)
       | x == "-n" =  parseArgs' (y:ys) (sargs {defOut = Just False})
       | otherwise = parseCmds x sargs >>= \sargs' -> parseArgs' (y:ys) sargs'
       where
         addCmd s@sargs x' = s {script = script s ++ ('\n':x')}

openFileError :: String -> E.IOException -> IO [a]
openFileError f e = putStr ("Error: Couldn't open " ++ f ++ ": " ++ 
                    show (e :: E.IOException)) >> return []

-- | Parse Sed program's embedded commands and the input files arguments
parseCmds :: String -> SedArgs -> IO SedArgs
parseCmds x s@(SedArgs files script _) = 
    if null script then return $ s {script = x} 
     else do 
       let (dir, fp) = splitFileName x
       fs <- G.globDir1 (G.compile fp) dir
       if null fs then error $ fp ++ ": No such file or directory"
        else return $ s {files = files ++ fs}

usage :: String
usage = unlines help 
  where help = ["usage: Hsed [-n] script [file...]",
                "       Hsed [-n] -e script [-e script]... [-f script_file]... [file...]",
                "       Hsed [-n] [-e script]... -f script_file [-f script_file]... [file...]"
               ]

