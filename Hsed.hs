-- |
-- Module      :  Main
-- Copyright   :  (c) Vitaliy Rkavishnikov
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

import System (getArgs)
import System.IO (putStrLn)
import qualified Control.Monad.State as S
import StreamEd (run)

main :: IO ()
main = do
    args <- getArgs
    if null args || head args == "--help" then do
      putStrLn usage
      return ()
     else run args

usage = unlines help 
  where help = ["usage: Hsed [-n] script [file...]",
                "       Hsed [-n] -e script [-e script]... [-f script_file]... [file...]",
                "       Hsed [-n] [-e script]... -f script_file [-f script_file]... [file...]"
               ]

