module PscPane.Interpreter where

import Prelude
import Control.Monad.Free (foldFree)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.State.Trans (StateT, lift, execStateT)
import Control.Monad.State.Class (get, modify)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Either (Either(..), either)
import Data.String (Pattern(..), Replacement(..), replace)
import Node.Path as Path
import PscIde (load, rebuild)
import PscIde.Command (RebuildResult(..))

import Blessed (render, setContent)
import PscPane.Config (Config)
import PscPane.Types (EffN, AffN)
import PscPane.Parser (readPscJson)
import PscPane.Pretty (formatState)
import PscPane.Output (display)
import PscPane.DSL (ActionF(..), Action)
import PscPane.State (PscFailure(..))
import PscPane.Spawn (spawn)


appN ∷ ActionF ~> StateT Config AffN
appN (RebuildModule path f) = do
  { port } ← get
  res ← lift $ rebuild port path
  either (throwError <<< error) (pure <<< f <<< takeOne) res

  where
  takeOne ∷ Either RebuildResult RebuildResult → Maybe PscFailure
  takeOne (Right (RebuildResult warnings)) = Warning <$> head warnings
  takeOne (Left (RebuildResult errors)) = Error <$> head errors

appN (LoadModules a) = do
  { port } ← get
  lift $ const a <$> load port [] []
appN (BuildProject f) = do
  { screen, box, options: { buildPath, srcPath, libPath, testPath, test } } ← get
  let srcGlob = Path.concat [srcPath, "**", "*.purs"]
      libGlob = Path.concat [libPath, "purescript-*", "src", "**", "*.purs"]
      testSrcGlob = Path.concat [testPath, "**", "*.purs"]
      args = ["--output", buildPath, "--json-errors", srcGlob, libGlob]
           <> if test then pure testSrcGlob else mempty
  res ← either _.stdErr _.stdErr <$> lift (spawn "psc" args)
  case readPscJson res of
    Nothing → throwError $ error "Could not read psc output."
    Just res' → pure (f res')
appN (RunTests f) = do
  { options: { buildPath, testMain } } ← get
  let modulePath = "./" <> buildPath <> "/" <> jsEscape testMain
  res ← lift $ spawn "node" ["-e", "require('" <> modulePath <> "').main();"]
  pure $ f res

  where
  -- | This is from bodil/pulp
  -- | 
  -- | Escape a string for insertion into a JS string literal.
  jsEscape :: String -> String
  jsEscape =
    replace (Pattern "'") (Replacement "\\'")
    <<< replace (Pattern "\\") (Replacement "'")

appN (ShouldRunTests f) = do
  { options: { test } } ← get
  pure (f test)
appN (DrawPaneState state a) = do
  { screen, box, cwd, options: { colorize } } ← get
  height ← liftEff rows
  liftEff (setContent box (formatState colorize cwd height state))
  liftEff (render screen)
  modify (_ { prevPaneState = state })
  pure a
appN (ShowError err a) = lift $ const a <$> display err

run ∷ ∀ a. Config → Action a → AffN Config
run state program = execStateT (foldFree appN program) state

foreign import rows ∷ EffN Int
