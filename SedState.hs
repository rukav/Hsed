{-# LANGUAGE TemplateHaskell #-}

module SedState where

import qualified Control.Monad.State as S
import qualified Data.Accessor.Basic as A
import Data.Accessor.Template (deriveAccessors)
import System.IO
import System.FilePath
import Ast

data Env = Env {
  ast_ :: [SedCmd],
  defOutput_ :: Bool,
  lastLine_ :: Int,
  curLine_ :: Int,
  inRange_ :: Bool,
  patternSpace_ :: String,
  holdSpace_ :: String,
  appendSpace_ :: [String],
  memorySpace_ :: String,
  useMemSpace_ :: Bool,
  exit_ :: Bool,
  fileout_ :: [(FilePath, Handle)],
  subst_ :: Bool
} deriving (Show)

$( deriveAccessors ''Env )

type SedState = S.StateT Env IO

initEnv = Env [] True 0 0 False [] [] [] [] False False [] False
--initEnv = Env [] [] True 0 0 False [] [] [] [] False False [] False

set :: A.T Env a -> a -> SedState ()
set f x = S.modify (A.set f x)

get :: A.T Env a -> SedState a
get f = S.gets (A.get f)

modify :: A.T Env a -> (a -> a) -> SedState ()
modify f g = S.modify (A.modify f g)
