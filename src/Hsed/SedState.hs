{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  SedState
-- Copyright   :  (c) Vitaliy Rkavishnikov
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The state of the program 

module Hsed.SedState where

import qualified Control.Monad.State as S
import qualified Data.Accessor.Basic as A
import qualified Data.ByteString.Char8 as B
import Data.Accessor.Template (deriveAccessors)
import System.IO
import Hsed.Ast

data Env = Env {
  ast_ :: [SedCmd],                 -- ^ Parsed Sed commands
  defOutput_ :: !Bool,              -- ^ Suppress the default output
  lastLine_ :: !Int,                -- ^ The last line index
  curLine_ :: !Int,                 -- ^ The current line index
  inRange_ :: !Bool,                -- ^ Is pattern space matches the address range
  patternSpace_ :: !B.ByteString,   -- ^ The buffer to keep the selected line(s)
  holdSpace_ :: !B.ByteString,      -- ^ The buffer to keep the line(s) temporarily
  appendSpace_ :: [B.ByteString],   -- ^ The buffer to keep the append lines
  memorySpace_ :: !B.ByteString,    -- ^ Store the output in the memory
  useMemSpace_ :: !Bool,            -- ^ If True the Sed output is stored in the memory buffer
  exit_ :: !Bool,                   -- ^ Exit the stream editor
  fileout_ :: [(FilePath, Handle)], -- ^ Write (w command) files handles 
  subst_ :: !Bool,                  -- ^ The result of the last substitution
  curFile_ :: (Handle, Bool)        -- ^ Current input file handle  
} deriving (Show)

$( deriveAccessors ''Env )

type SedState = S.StateT Env IO

initEnv :: Env
initEnv = Env {
  ast_ = [],
  defOutput_ = True,
  lastLine_ = 0,
  curLine_ = 0,
  inRange_ = False,
  patternSpace_ = B.empty,
  holdSpace_ = B.empty,
  appendSpace_ = [],
  memorySpace_ = B.empty,
  useMemSpace_ = False,
  exit_  = False,
  fileout_ = [],
  subst_ = False,
  curFile_ = (stdin, True)
}

set :: A.T Env a -> a -> SedState ()
set f x = S.modify (A.set f x)

get :: A.T Env a -> SedState a
get f = S.gets (A.get f)

modify :: A.T Env a -> (a -> a) -> SedState ()
modify f g = S.modify (A.modify f g)
