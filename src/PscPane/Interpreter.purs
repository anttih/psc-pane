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
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Node.Path as Path
import Node.Process as P
import PscIde (load, rebuild)
import PscIde.Command (RebuildResult(..))
import PscIde.Server (stopServer)
import PscPane.Config (Config)
import PscPane.DSL (ActionF(..), Action)
import PscPane.Output (display)
import PscPane.Parser (readPscJson)
import PscPane.Pretty (formatState)
import PscPane.Spawn (spawn)
import PscPane.State (PscFailure(..))

appN ∷ ActionF ~> StateT Config Aff
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
      args = ["build", "--", "--output", buildPath, "--json-errors"]
           <> if test then pure testSrcGlob else mempty
  res ← either _.stdErr _.stdErr <$> lift (spawn "psc-package" args)
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

appN (DrawPaneState state a) = do
  { screen, box, cwd, options: { colorize } } ← get
  height ← liftEffect rows
  liftEffect (setContent box (formatState colorize cwd height state))
  liftEffect (render screen)
  void $ modify (_ { prevPaneState = state })
  pure a
appN (ShowError err a) = lift $ const a <$> display err
appN (Exit next) = do
  { port } <- get
  void $ lift $ attempt $ stopServer port
  void $ liftEffect $ P.exit 0
  pure next
appN (Ask f) = do
  config <- get
  pure (f config)

run ∷ ∀ a. Config → Action a → Aff Config
run state program = execStateT (foldFree appN program) state

foreign import rows ∷ Effect Int
