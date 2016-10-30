module PscPane.DSL where

import Prelude
import Control.Monad.Free (Free, liftF)
import Data.Either (Either)
import PscIde.Command (RebuildResult)
import Data.Maybe (Maybe)

import PscPane.State (State)
import PscPane.Parser(PscResult)

data ActionF a
  = RebuildModule String (Either RebuildResult RebuildResult → a)
  | LoadModules a
  | BuildProject (PscResult → a)
  | DrawPaneState State a
  | ShowError String a
  | RunTests (Maybe String → a)
  | ShouldRunTests (Boolean → a)
  
type Action a = Free ActionF a

rebuildModule ∷ String → Action (Either RebuildResult RebuildResult)
rebuildModule path = liftF (RebuildModule path id)

loadModules ∷ Action Unit
loadModules = liftF (LoadModules unit)

buildProject ∷ Action PscResult
buildProject = liftF (BuildProject id)

runTests ∷ Action (Maybe String)
runTests = liftF (RunTests id)

shouldRunTests ∷ Action Boolean
shouldRunTests = liftF (ShouldRunTests id)

drawPaneState ∷ State → Action Unit
drawPaneState state = liftF (DrawPaneState state unit)

showError ∷ String → Action Unit
showError err = liftF (ShowError err unit)
