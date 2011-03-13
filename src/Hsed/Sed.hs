-- |
-- Module      :  Sed
-- Copyright   :  (c) Vitaliy Rukavishnikov
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides functions to execute the sed script. 
-- See 'execScript' below for an example

module Hsed.Sed 
 ( execScript
 , execScript_
 , SedScript
 ) where

import qualified Data.ByteString.Char8 as B
import Hsed.StreamEd (runSed)
import Hsed.SedState

type SedScript = String

-- | Execute the sed script and print the output to ByteString. 
-- Example. Suppose the ../tests/Transform.in file contains the line 'Hello world!'.
-- execScript [\"../tests/Transform.in\"] \"y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/\"
-- will produce 'HELLO WORLD!' bytestring.
execScript :: [FilePath] -> SedScript -> IO B.ByteString
execScript files script = do
   env <- runSed files script (initEnv {useMemSpace_ = True})
   return $ memorySpace_ env

-- | Execute the sed script and print the result to stdout 
execScript_ :: [FilePath] -> SedScript -> IO ()
execScript_ files script = do
   runSed files script initEnv
   return ()







