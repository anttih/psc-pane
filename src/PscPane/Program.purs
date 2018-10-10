module PscPane.Program where

import Prelude

import Data.Either (Either(..), either)
import Data.Foldable (any)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (Pattern(..), Replacement(..), replace)
import Node.Path as Path
import PscPane.DSL (Action, ask, exit)
import PscPane.DSL as A
import PscPane.DSL as Reason
import PscPane.Parser (readPscJson)
import PscPane.Program (Event(FileChange, Resize, Quit, Init), minimatch)
import PscPane.Spawn (SpawnOutput)
import PscPane.State (Progress(..), PscFailure, State(..))

data Event = Init | Resize | Quit | FileChange String

foreign import minimatch ∷ String → String → Boolean

eval :: Event -> Action Unit
eval = case _ of
  Init ->
    initialBuild

  Quit ->
    exit Reason.Quit

  Resize -> do
    { prevPaneState } <- ask
    A.drawPaneState prevPaneState

  FileChange path -> do
    when (any (minimatch path) ["**/*.purs"]) (rebuildModule path)
    -- Changing a .js file triggers a full build for now. Should we just
    -- build and load the purs file?
    when (any (minimatch path) ["**/*.js"]) buildProject

  where

  buildProject ∷ A.Action Unit
  buildProject = do
    err ← runBuildCmd
    A.loadModules
    case err of
      Just res →
        A.drawPaneState (PscError res)
      Nothing → do
        runTests ← shouldRunTests
        if runTests then
          do
            A.drawPaneState (BuildSuccess (InProgress "running tests..."))
            testResult ← runTestCmd
            case testResult of
              Left out → A.drawPaneState (TestFailure out)
              Right _ → A.drawPaneState TestSuccess
          else
            A.drawPaneState (BuildSuccess Done)

    where

    runBuildCmd :: A.Action (Maybe PscFailure)
    runBuildCmd = do
      { options: { buildPath, srcPath, libPath, testPath, test } } ← ask
      let srcGlob = Path.concat [srcPath, "**", "*.purs"]
          libGlob = Path.concat [libPath, "purescript-*", "src", "**", "*.purs"]
          testSrcGlob = Path.concat [testPath, "**", "*.purs"]
          args = ["build", "--", "--output", buildPath, "--json-errors"]
              <> if test then pure testSrcGlob else mempty
      res ← either _.stdErr _.stdErr <$> A.spawn "psc-package" args
      case readPscJson res of
        Left err → A.exit (Reason.Error ("Could not read psc output: " <> err))
        Right res' → pure res'

    runTestCmd :: A.Action (Either SpawnOutput SpawnOutput)
    runTestCmd = do
      { options: { buildPath, testMain } } ← ask
      let modulePath = "./" <> buildPath <> "/" <> jsEscape testMain
      A.spawn "node" ["-e", "require('" <> modulePath <> "').main();"]

      where
      -- | This is from bodil/pulp
      -- |
      -- | Escape a string for insertion into a JS string literal.
      jsEscape :: String -> String
      jsEscape =
        replace (Pattern "'") (Replacement "\\'")
        <<< replace (Pattern "\\") (Replacement "'")

  initialBuild ∷ A.Action Unit
  initialBuild = do
    A.drawPaneState InitialBuild
    buildProject

  rebuildModule ∷ String → A.Action Unit
  rebuildModule path = do
    A.drawPaneState (CompilingModule path)
    firstErr ← A.rebuildModule path
    rebuild ← shouldBuildAll
    A.drawPaneState (toPaneState firstErr rebuild)
    when (isNothing firstErr && rebuild) buildProject

    where
    toPaneState ∷ Maybe PscFailure → Boolean → State
    toPaneState (Just res) _ = PscError res
    toPaneState Nothing true = ModuleOk path (InProgress "building project...")
    toPaneState Nothing false = ModuleOk path Done

  shouldRunTests :: A.Action Boolean
  shouldRunTests = do
    { options: { test } } ← ask
    pure test

  shouldBuildAll :: A.Action Boolean
  shouldBuildAll = do
    { options: { rebuild }} ← ask
    pure rebuild