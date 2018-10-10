module PscPane.DSL where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Either (Either)
import Data.Maybe (Maybe)
import PscPane.Config (Config)
import PscPane.Spawn (SpawnOutput)
import PscPane.State (State, PscFailure)

data ExitReason = Quit | Error String

data ActionF a
  = RebuildModule String (Maybe PscFailure → a)
  | LoadModules a
  | DrawPaneState State a
  | ShowError String a
  | Exit ExitReason
  | Ask (Config -> a)
  | Spawn String (Array String) (Either SpawnOutput SpawnOutput -> a)

type DSL a = Free ActionF a

rebuildModule ∷ String → DSL (Maybe PscFailure)
rebuildModule path = liftF (RebuildModule path identity)

loadModules ∷ DSL Unit
loadModules = liftF (LoadModules unit)

drawPaneState ∷ State → DSL Unit
drawPaneState state = liftF (DrawPaneState state unit)

showError ∷ String → DSL Unit
showError err = liftF (ShowError err unit)

exit :: forall a. ExitReason -> DSL a
exit reason = liftF (Exit reason)

ask :: DSL Config
ask = liftF (Ask identity)

spawn :: String -> Array String -> DSL (Either SpawnOutput SpawnOutput)
spawn command args = liftF (Spawn command args identity)