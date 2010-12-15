module Main where

import System
import System.IO 
import System.FilePath
import qualified System.FilePath.Glob as G
import qualified Control.Monad.State as S
import Control.Monad (when)
import qualified Control.Exception as E
import Data.List (isPrefixOf)
import StreamEd
import SedState
import Parsec

main :: IO ()
main = do
    args <- getArgs
    if null args || head (args) == "--help" then do
      putStrLn usage
      return ()
     else do
      S.execStateT ( do 
                      (files, cmds) <- parseArgs args
                      compile cmds
                      execute files
                   ) initEnv
      return ()

parseArgs :: [String] -> SedState ([FilePath], String)
parseArgs xs = parseArgs' xs ([],[]) where
    parseArgs' [] fs = return fs
    parseArgs' [x] fs 
       | x == "-n" = set defOutput False >> return fs
       | otherwise = noFlag x fs
    parseArgs' (x:y:xs) fs
       | x == "-e" = parseArgs' xs (addCmd fs y)
       | x == "-f" = do
            sed <- S.lift $ System.IO.readFile y `catch` openFileError y
            when ("#n" `isPrefixOf` sed) $ set defOutput False 
            parseArgs' xs (addCmd fs sed)
       | x == "-n" = set defOutput False >> parseArgs' (y:xs) fs
       | otherwise = noFlag x fs >>= \fs' -> parseArgs' (y:xs) fs'
       where
         addCmd (files, cmds) x = (files, cmds ++ ('\n':x))

usage = unlines help 
  where help = ["usage: Sedhs [-n] script [file...]",
                "       Sedhs [-n] -e script [-e script]... [-f script_file]... [file...]",
                "       Sedhs [-n] [-e script]... -f script_file [-f script_file]... [file...]"
               ]

noFlag :: String -> ([FilePath], String) -> SedState ([FilePath], String)
noFlag x (files, cmds) = 
   if null cmds then return (files, x) 
    else addFiles x (files,cmds)

addFiles :: String -> ([FilePath], String) -> SedState ([FilePath], String)
addFiles x (files, cmds) = do
   let (dir, fp) = splitFileName x
   fs <- S.lift $ G.globDir1 (G.compile fp) dir
   if null fs then 
      E.throw (ExecutionError $ fp ++ ": No such file or directory")
    else return (files ++ fs, cmds)

openFileError f e = 
  putStr ("Error: Couldn't open " ++ f ++ ": " ++ show (e :: E.IOException)) >>
  return ""