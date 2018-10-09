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
  | BuildProject (Maybe PscFailure → a)
  | DrawPaneState State a
  | ShowError String a
  | RunTests (Either SpawnOutput SpawnOutput → a)
  | Exit ExitReason a
  | Ask (Config -> a)

type Action a = Free ActionF a

rebuildModule ∷ String → Action (Maybe PscFailure)
rebuildModule path = liftF (RebuildModule path identity)

loadModules ∷ Action Unit
loadModules = liftF (LoadModules unit)

buildProject ∷ Action (Maybe PscFailure)
buildProject = liftF (BuildProject identity)

runTests ∷ Action (Either SpawnOutput SpawnOutput)
runTests = liftF (RunTests identity)

drawPaneState ∷ State → Action Unit
drawPaneState state = liftF (DrawPaneState state unit)

showError ∷ String → Action Unit
showError err = liftF (ShowError err unit)

exit :: ExitReason -> Action Unit
exit reason = liftF (Exit reason unit)

ask :: Action Config
ask = liftF (Ask identity)