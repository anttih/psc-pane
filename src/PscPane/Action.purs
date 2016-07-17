module PscPane.Action where
  
import Prelude
import Control.Monad.Aff (makeAff)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
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
  , dir ∷ String
  , buildCmd ∷ String
  , screen ∷ Screen
  , box ∷ Box
  }

appN ∷ State → (ActionF ~> AffN)
appN { port } (RebuildModule path f) = do
  res ← rebuild port path
  either (throwError <<< error) (pure <<< f) res
appN { port } (LoadModules a) = const a <$> load port [] []
appN { screen, box, buildCmd } (BuildProject f) = do
  buf ← spawnAff buildCmd
  liftEff $ f <$> toString UTF8 buf
appN { screen, box, dir } (DrawPaneState state a) = do
  height ← liftEff rows
  liftEff (setContent box (formatState dir height state))
  liftEff (render screen)
  pure a
appN _ (ShowError err a) = const a <$> display err

run ∷ State → Action ~> AffN
run state = foldFree (appN state)

foreign import spawn :: Fn2 String (Buffer → EffN Unit) (EffN Unit)

spawnAff :: String → AffN Buffer
spawnAff cmd = makeAff (\error success → runFn2 spawn cmd success)

foreign import rows :: EffN Int
