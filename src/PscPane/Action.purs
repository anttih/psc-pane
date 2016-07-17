module PscPane.Action where
  
import Prelude
import Control.Monad.Aff (makeAff)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.State.Trans (StateT, lift, execStateT)
import Control.Monad.State.Class (get, modify)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Either (Either, either)
import Node.Buffer (Buffer, toString)
import Node.Encoding (Encoding(UTF8))

import Blessed (Screen, Box, render, setContent)

import PscIde (load, rebuild)
import PscIde.Command (RebuildResult)

import PscPane.Types (EffN, AffN)
import PscPane.Pretty (PaneState, formatState)
import PscPane.Output (display)

data ActionF a
  = RebuildModule String (Either RebuildResult RebuildResult → a)
  | LoadModules a
  | BuildProject (String → a)
  | DrawPaneState PaneState a
  | ShowError String a
  
type Action a = Free ActionF a

rebuildModule ∷ String → Action (Either RebuildResult RebuildResult)
rebuildModule path = liftF (RebuildModule path id)

loadModules ∷ Action Unit
loadModules = liftF (LoadModules unit)

buildProject ∷ Action String
buildProject = liftF (BuildProject id)

drawPaneState ∷ PaneState → Action Unit
drawPaneState state = liftF (DrawPaneState state unit)

showError ∷ String → Action Unit
showError err = liftF (ShowError err unit)

type State =
  { port ∷ Int
  , cwd ∷ String
  , buildCmd ∷ String
  , screen ∷ Screen
  , box ∷ Box
  , prevPaneState ∷ PaneState
  }

getState ∷ StateT State AffN State
getState = get

appN ∷ ActionF ~> StateT State AffN
appN (RebuildModule path f) = do
  { port } ← getState
  res ← lift $ rebuild port path
  either (throwError <<< error) (pure <<< f) res
appN (LoadModules a) = do
  { port } ← getState
  lift $ const a <$> load port [] []
appN (BuildProject f) = do
  { screen, box, buildCmd } ← getState
  buf ← lift $ spawnAff buildCmd
  liftEff $ f <$> toString UTF8 buf
appN (DrawPaneState state a) = do
  { screen, box, cwd } ← getState
  height ← liftEff rows
  liftEff (setContent box (formatState cwd height state))
  liftEff (render screen)
  modify (_ { prevPaneState = state })
  pure a
appN (ShowError err a) = lift $ const a <$> display err

run ∷ ∀ a. State → Action a → AffN State
run state program = execStateT (foldFree appN program) state

foreign import spawn :: Fn2 String (Buffer → EffN Unit) (EffN Unit)

spawnAff :: String → AffN Buffer
spawnAff cmd = makeAff (\error success → runFn2 spawn cmd success)

foreign import rows :: EffN Int
