module PscPane.Program where

import Prelude

import Data.Either (Either(..), either)
import Data.Foldable (any)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (Pattern(..), Replacement(..), replace)
import Node.Path as Path
import PscPane.Config (Config)
import PscPane.Parser (readPscJson)
import PscPane.Spawn (SpawnOutput)
import PscPane.State (Progress(..), PscFailure, State(..))
import Run (FProxy, Run, SProxy(..))
import Run as Run
import Run.Except (EXCEPT, _except, throwAt)

data ExitReason = Exit | Error String

data ActionF a
  = RebuildModule String (Maybe PscFailure → a)
  | LoadModules a
  | DrawPaneState State a
  | ShowError String a
  | Ask (Config -> a)
  | Spawn String (Array String) (Either SpawnOutput SpawnOutput -> a)

derive instance functorTalkF :: Functor ActionF

type ACTION = FProxy ActionF

_action = SProxy :: SProxy "action"

type DSL r a = Run (action :: ACTION, except :: EXCEPT ExitReason | r) a

rebuildModule' ∷ forall r. String → DSL r (Maybe PscFailure)
rebuildModule' path = Run.lift _action (RebuildModule path identity)

loadModules ∷ forall r.  DSL r Unit
loadModules = Run.lift _action (LoadModules unit)

drawPaneState ∷ forall r. State → DSL r Unit
drawPaneState state = Run.lift _action (DrawPaneState state unit)

showError ∷ forall r. String → DSL r Unit
showError err = Run.lift _action (ShowError err unit)

ask :: forall r. DSL r Config
ask = Run.lift _action (Ask identity)

spawn :: forall r. String -> Array String -> DSL r (Either SpawnOutput SpawnOutput)
spawn command args = Run.lift _action (Spawn command args identity)



data Event = Init | Resize | Quit | FileChange String

foreign import minimatch ∷ String → String → Boolean

eval :: forall r. Event -> DSL r Unit
eval = case _ of
  Init ->
    initialBuild

  Quit ->
    exit Exit

  Resize -> do
    { prevPaneState } <- ask
    drawPaneState prevPaneState

  FileChange path -> do
    when (any (minimatch path) ["**/*.purs"]) (rebuildModule path)
    -- Changing a .js file triggers a full build for now. Should we just
    -- build and load the purs file?
    when (any (minimatch path) ["**/*.js"]) buildProject

buildProject ∷ forall r. DSL r Unit
buildProject = do
  err ← runBuildCmd
  loadModules
  case err of
    Just res →
      drawPaneState (PscError res)
    Nothing → do
      runTests ← shouldRunTests
      if runTests then
        do
          drawPaneState (BuildSuccess (InProgress "running tests..."))
          testResult ← runTestCmd
          case testResult of
            Left out → drawPaneState (TestFailure out)
            Right _ → drawPaneState TestSuccess
        else
          drawPaneState (BuildSuccess Done)

runBuildCmd :: forall r. DSL r (Maybe PscFailure)
runBuildCmd = do
  { options: { buildPath, srcPath, libPath, testPath, test } } ← ask
  let srcGlob = Path.concat [srcPath, "**", "*.purs"]
      libGlob = Path.concat [libPath, "purescript-*", "src", "**", "*.purs"]
      testSrcGlob = Path.concat [testPath, "**", "*.purs"]
      args = ["build", "--", "--output", buildPath, "--json-errors"]
          <> if test then pure testSrcGlob else mempty
  res ← either _.stdErr _.stdErr <$> spawn "psc-package" args
  case readPscJson res of
    Left err → exit (Error ("Could not read psc output: " <> err))
    Right res' → pure res'

runTestCmd :: forall r. DSL r (Either SpawnOutput SpawnOutput)
runTestCmd = do
  { options: { buildPath, testMain } } ← ask
  let modulePath = "./" <> buildPath <> "/" <> jsEscape testMain
  spawn "node" ["-e", "require('" <> modulePath <> "').main();"]

  where
  -- | This is from bodil/pulp
  -- |
  -- | Escape a string for insertion into a JS string literal.
  jsEscape :: String -> String
  jsEscape =
    replace (Pattern "'") (Replacement "\\'")
    <<< replace (Pattern "\\") (Replacement "'")

initialBuild ∷ forall r. DSL r Unit
initialBuild = do
  drawPaneState InitialBuild
  buildProject

rebuildModule ∷ forall r. String → DSL r Unit
rebuildModule path = do
  drawPaneState (CompilingModule path)
  firstErr ← rebuildModule' path
  rebuild ← shouldBuildAll
  drawPaneState (toPaneState firstErr rebuild)
  when (isNothing firstErr && rebuild) buildProject

  where
  toPaneState ∷ Maybe PscFailure → Boolean → State
  toPaneState (Just res) _ = PscError res
  toPaneState Nothing true = ModuleOk path (InProgress "building project...")
  toPaneState Nothing false = ModuleOk path Done

shouldRunTests :: forall r. DSL r Boolean
shouldRunTests = do
  { options: { test } } ← ask
  pure test

shouldBuildAll :: forall r. DSL r Boolean
shouldBuildAll = do
  { options: { rebuild }} ← ask
  pure rebuild

exit :: forall r a. ExitReason -> DSL r a
exit = throwAt _except
