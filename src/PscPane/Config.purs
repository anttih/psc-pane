module PscPane.Config
  ( Config
  ) where

import Blessed (Screen, Box)
import PscPane.State (State)

type Config =
  { port ∷ Int
  , cwd ∷ String
  , srcPath ∷ String
  , libPath ∷ String
  , testPath ∷ String
  , testMain ∷ String
  , test ∷ Boolean
  , screen ∷ Screen
  , box ∷ Box
  , prevPaneState ∷ State
  , colorize ∷ Boolean
  }
