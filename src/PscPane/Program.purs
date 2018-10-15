module PscPane.Program where

import Prelude

import Blessed (setContent)
import Blessed as Blessed
import Data.Array (head)
import Data.Either (Either(..), either)
import Data.Foldable (any)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (Pattern(..), Replacement(..), replace)
import Effect (Effect)
import Effect.Aff (attempt, error, throwError)
import Node.Path as Path
import PscIde (load, rebuild) as Ide
import PscIde.Command (RebuildResult(..))
import PscIde.Server (stopServer) as Ide
import PscPane.Config (Config)
import PscPane.Parser (readPscJson)
import PscPane.Pretty (formatState)
import PscPane.Spawn (SpawnOutput)
import PscPane.Spawn as Spawn
import PscPane.State (Progress(..), PscFailure, State(..))
import PscPane.State as PscFailure
import Run (AFF, FProxy, Run, SProxy(..), EFFECT)
import Run as Run
import Run.Except (EXCEPT, _except, throwAt)
import Run.State (STATE)
import Run.State as State

data ExitReason = Exit | Error String

type DSL r a = Run (aff :: AFF, effect :: EFFECT, state :: STATE Config, except :: EXCEPT ExitReason | r) a

data Event = Init | Resize | Quit | FileChange String


eval :: forall r. Event -> DSL r Unit
eval = case _ of
  Init ->
    initialBuild

  Quit -> do
    { port } <- State.get
    void $ Run.liftAff $ attempt $ Ide.stopServer port
    exit Exit

  Resize -> do
    { prevPaneState } <- State.get
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
  { options: { buildPath, srcPath, libPath, testPath, test } } ← State.get
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
  { options: { buildPath, testMain } } ← State.get
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

loadModules ∷ forall r. DSL r Unit
loadModules = do
  { port } ← State.get
  void $ Run.liftAff (Ide.load port [] [])

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

  rebuildModule' :: String -> DSL r (Maybe PscFailure)
  rebuildModule' path' = do
    { port } <- State.get
    res ← Run.liftAff $ Ide.rebuild port path' Nothing
    either (Run.liftAff <<< throwError <<< error) (pure <<< takeOne) res

  takeOne ∷ Either RebuildResult RebuildResult → Maybe PscFailure
  takeOne (Right (RebuildResult warnings)) = PscFailure.Warning <$> head warnings
  takeOne (Left (RebuildResult errors)) = PscFailure.Error <$> head errors

drawPaneState ∷ forall r. State -> DSL r Unit
drawPaneState state = do
  { screen, box, cwd, options: { colorize } } <- State.get
  height ← Run.liftEffect rows
  Run.liftEffect (Blessed.setContent box (formatState colorize cwd height state))
  Run.liftEffect (Blessed.render screen)
  void $ State.modify (_ { prevPaneState = state })

spawn :: forall r. String -> Array String -> DSL r (Either SpawnOutput SpawnOutput)
spawn command args = Run.liftAff $ Spawn.spawn command args

shouldRunTests :: forall r. DSL r Boolean
shouldRunTests = do
  { options: { test } } ← State.get
  pure test

shouldBuildAll :: forall r. DSL r Boolean
shouldBuildAll = do
  { options: { rebuild }} ← State.get
  pure rebuild

exit :: forall r a. ExitReason -> DSL r a
exit = throwAt _except

foreign import minimatch ∷ String → String → Boolean

foreign import rows ∷ Effect Int