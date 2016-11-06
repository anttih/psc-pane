module PscPane.Spawn where

import Prelude
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Node.ChildProcess as CP
import Node.ChildProcess (CHILD_PROCESS, Exit(BySignal, Normally), defaultSpawnOptions,
                          onClose, stdout, stderr)
import Node.Encoding (Encoding(UTF8))
import Node.Stream (onDataString)

type SpawnOutput = { stdOut ∷ String, stdErr ∷ String }

spawn ∷ ∀ eff
  . String
  → Array String
  → Aff (ref ∷ REF, err ∷ EXCEPTION, cp ∷ CHILD_PROCESS | eff) (Either SpawnOutput SpawnOutput)
spawn exe args = do
  child ← liftEff $ CP.spawn exe args defaultSpawnOptions
  errRef ← liftEff $ newRef ""
  stdRef ← liftEff $ newRef ""
  makeAff \_ succ -> do
    onDataString (stdout child) UTF8 \s → do
      current ← readRef stdRef
      writeRef stdRef (current <> s)

    onDataString (stderr child) UTF8 \s → do
      current ← readRef errRef
      writeRef errRef (current <> s)

    onClose child \c → case c of
      (Normally 0) → do
        stdOut ← readRef stdRef
        stdErr ← readRef errRef
        succ $ Right { stdOut, stdErr }
      (Normally n) → do
        stdOut ← readRef stdRef
        stdErr ← readRef errRef
        succ $ Left { stdOut, stdErr }
      (BySignal _) → succ $ Left { stdErr: "Signal interrupted test", stdOut: "" }
