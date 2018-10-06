module PscPane.Spawn where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, error, makeAff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Node.ChildProcess (Exit(BySignal, Normally), defaultSpawnOptions, onClose, stdout, stderr)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(UTF8))
import Node.Stream (onDataString)

type SpawnOutput = { stdOut ∷ String, stdErr ∷ String }

spawn ∷ String → Array String → Aff (Either SpawnOutput SpawnOutput)
spawn exe args = do
  child ← liftEffect $ CP.spawn exe args defaultSpawnOptions
  errRef ← liftEffect $ Ref.new ""
  stdRef ← liftEffect $ Ref.new ""
  makeAff \succ -> do
    onDataString (stdout child) UTF8 \s → do
      current ← Ref.read stdRef
      Ref.write (current <> s) stdRef

    onDataString (stderr child) UTF8 \s → do
      current ← Ref.read errRef
      Ref.write (current <> s) errRef

    onClose child \c → case c of
      (Normally 0) → do
        stdOut ← Ref.read stdRef
        stdErr ← Ref.read errRef
        succ $ pure $ Right { stdOut, stdErr }
      (Normally n) → do
        stdOut ← Ref.read stdRef
        stdErr ← Ref.read errRef
        succ $ pure $ Left { stdOut, stdErr }
      (BySignal _) → succ $ Left (error "Signal interrupted test")
    pure $ Aff.nonCanceler
