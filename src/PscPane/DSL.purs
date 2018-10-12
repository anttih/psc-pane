module PscPane.DSL where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import PscPane.Config (Config)
import PscPane.Spawn (SpawnOutput)
import PscPane.State (State, PscFailure)
import Run (FProxy, Run, SProxy(..))
import Run as Run

data ExitReason = Quit | Error String

data ActionF a
  = RebuildModule String (Maybe PscFailure → a)
  | LoadModules a
  | DrawPaneState State a
  | ShowError String a
  | Exit ExitReason
  | Ask (Config -> a)
  | Spawn String (Array String) (Either SpawnOutput SpawnOutput -> a)

derive instance functorTalkF :: Functor ActionF

type ACTION = FProxy ActionF

_action = SProxy :: SProxy "action"

type DSL r a = Run (action :: ACTION | r) a

rebuildModule ∷ forall r. String → DSL r (Maybe PscFailure)
rebuildModule path = Run.lift _action (RebuildModule path identity)

loadModules ∷forall r.  DSL r Unit
loadModules = Run.lift _action (LoadModules unit)

drawPaneState ∷ forall r. State → DSL r Unit
drawPaneState state = Run.lift _action (DrawPaneState state unit)

showError ∷ forall r. String → DSL r Unit
showError err = Run.lift _action (ShowError err unit)

exit :: forall r a. ExitReason -> DSL r a
exit reason = Run.lift _action (Exit reason)

ask :: forall r. DSL r Config
ask = Run.lift _action (Ask identity)

spawn :: forall r. String -> Array String -> DSL r (Either SpawnOutput SpawnOutput)
spawn command args = Run.lift _action (Spawn command args identity)