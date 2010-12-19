module Main where

import System
import System.IO 
import qualified Control.Monad.State as S
import StreamEd
import SedState

main :: IO ()
main = do
    args <- getArgs
    print args
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

usage = unlines help 
  where help = ["usage: Sed [-n] script [file...]",
                "       Sed [-n] -e script [-e script]... [-f script_file]... [file...]",
                "       Sed [-n] [-e script]... -f script_file [-f script_file]... [file...]"
               ]

