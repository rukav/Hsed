-- |
-- Module      :  Sed
-- Copyright   :  (c) Vitaliy Rkavishnikov
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides functions to execute the sed script
-- 

module Sed 
 ( execScript
 , execScript'
 , SedScript
 ) where

import Control.Monad (when)
import qualified Control.Monad.State as S
import Data.List (isPrefixOf)
import qualified Data.ByteString.Char8 as B
import StreamEd (compile, execute)
import SedState

type SedScript = String

-- | Execute the sed script and print the result to stdout 
execScript :: [FilePath] -> SedScript -> IO ()
execScript files script = do
   execSed files script initEnv
   return ()

-- | Execute the sed script and print the output to ByteString
execScript' :: [FilePath] -> SedScript -> IO B.ByteString
execScript' files script = do
   env <- execSed files script (initEnv {useMemSpace_ = True})
   return $ memorySpace_ env

-- | Compile and execute the sed script
execSed :: [FilePath] -> SedScript -> Env -> IO Env
execSed files script = 
   S.execStateT (
     when ("#n" `isPrefixOf` script) (set defOutput False) >>
     compile script >> execute files
   )


