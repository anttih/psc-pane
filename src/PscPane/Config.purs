module PscPane.Config
  ( Config
  , Options
  ) where

import Blessed (Screen, Box)
import PscPane.State (State)

type Options =
  { buildPath ∷ String
  , srcPath ∷ String
  , libPath ∷ String
  , testPath ∷ String
  , testMain ∷ String
  , test ∷ Boolean
  , colorize ∷ Boolean
  }

type Config =
  { port ∷ Int
  , cwd ∷ String
  , screen ∷ Screen
  , box ∷ Box
  , prevPaneState ∷ State
  , options ∷ Options
  }
