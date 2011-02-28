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

module Hsed.Sed 
 ( execScript
 , execScript_
 , SedScript
 ) where

import Control.Monad (when)
import qualified Control.Monad.State as S
import Data.List (isPrefixOf)
import qualified Data.ByteString.Char8 as B
import Hsed.StreamEd (runSed)
import Hsed.SedState

type SedScript = String

-- | Execute the sed script and print the output to ByteString
execScript :: [FilePath] -> SedScript -> IO B.ByteString
execScript files script = do
   env <- runSed files script (initEnv {useMemSpace_ = True})
   return $ memorySpace_ env

-- | Execute the sed script and print the result to stdout 
execScript_ :: [FilePath] -> SedScript -> IO ()
execScript_ files script = do
   runSed files script initEnv
   return ()







