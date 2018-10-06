module PscPane.Interpreter where

import Prelude

import Blessed (render, setContent)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Free (foldFree)
import Control.Monad.State.Class (get, modify)
import Control.Monad.State.Trans (StateT, lift, execStateT)
import Data.Array (head)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replace)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Node.Path as Path
import PscIde (load, rebuild)
import PscIde.Command (RebuildResult(..))
import PscPane.Config (Config)
import PscPane.DSL (ActionF(..), Action)
import PscPane.Output (display)
import PscPane.Parser (readPscJson)
import PscPane.Pretty (formatState)
import PscPane.Spawn (spawn)
import PscPane.State (PscFailure(..))
import PscPane.Types (EffN, AffN)


appN ∷ ActionF ~> StateT Config AffN
appN (RebuildModule path f) = do
  { port } ← get
  res ← lift $ rebuild port path Nothing
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
      args = ["compile", "--output", buildPath, "--json-errors", srcGlob, libGlob]
           <> if test then pure testSrcGlob else mempty
  res ← either _.stdErr _.stdErr <$> lift (spawn "purs" args)
  case readPscJson res of
    Left err → throwError $ error $ "Could not read psc output: " <> err
    Right res' → pure (f res')
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
appN (ShouldBuildAll f) = do
  { options: { rebuild }} ← get
  pure (f rebuild)
appN (DrawPaneState state a) = do
  { screen, box, cwd, options: { colorize } } ← get
  height ← liftEffect rows
  liftEffect (setContent box (formatState colorize cwd height state))
  liftEffect (render screen)
  void $ modify (_ { prevPaneState = state })
  pure a
appN (ShowError err a) = lift $ const a <$> display err

run ∷ ∀ a. Config → Action a → AffN Config
run state program = execStateT (foldFree appN program) state

foreign import rows ∷ EffN Int
