module PscPane.DSL where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Either (Either)
import Data.Maybe (Maybe)
import PscPane.Config (Config)
import PscPane.Spawn (SpawnOutput)
import PscPane.State (State, PscFailure)

data ActionF a
  = RebuildModule String (Maybe PscFailure → a)
  | LoadModules a
  | BuildProject (Maybe PscFailure → a)
  | DrawPaneState State a
  | ShowError String a
  | RunTests (Either SpawnOutput SpawnOutput → a)
  | Exit a
  | Ask (Config -> a)
  | ShouldBuildAll (Boolean → a)

type Action a = Free ActionF a

rebuildModule ∷ String → Action (Maybe PscFailure)
rebuildModule path = liftF (RebuildModule path identity)

loadModules ∷ Action Unit
loadModules = liftF (LoadModules unit)

buildProject ∷ Action (Maybe PscFailure)
buildProject = liftF (BuildProject identity)

runTests ∷ Action (Either SpawnOutput SpawnOutput)
runTests = liftF (RunTests identity)

shouldBuildAll ∷ Action Boolean
shouldBuildAll = liftF (ShouldBuildAll identity)

drawPaneState ∷ State → Action Unit
drawPaneState state = liftF (DrawPaneState state unit)

showError ∷ String → Action Unit
showError err = liftF (ShowError err unit)

exit :: Action Unit
exit = liftF (Exit unit)

ask :: Action Config
ask = liftF (Ask identity)