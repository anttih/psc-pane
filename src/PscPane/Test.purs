module PscPane.Test where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.Aff (makeAff)
import Data.Maybe (Maybe(Nothing, Just))
import Node.Encoding (Encoding(UTF8))
import Node.ChildProcess (Exit(BySignal, Normally), spawn, defaultSpawnOptions, onClose, stdout)
import Node.Stream (onDataString)

import PscPane.Types (AffN)

runTests ∷ String → AffN (Maybe String)
runTests cmd = do
  ref ← liftEff $ newRef ""
  child ← liftEff $ spawn "pulp" ["test", "--no-psa"] defaultSpawnOptions
  makeAff \_ succ -> do
    onDataString (stdout child) UTF8 \s → do
      current ← readRef ref
      writeRef ref (current <> s)

    onClose child \c → case c of
      (Normally 0) → readRef ref >>= const (succ Nothing)
      (Normally n) → readRef ref >>= succ <<< Just
      (BySignal _) → succ $ Just "Signal interrupted test"

