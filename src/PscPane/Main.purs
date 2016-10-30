module PscPane.Main where

import PscPane.Action as A
import Blessed (onResize, onQuit, render, append, setContent, mkBox, mkScreen)
import Control.Alt ((<|>))
import Control.Coroutine (Consumer, runProcess, consumer, ($$))
import Control.Monad.Aff (runAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, error, message)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.Error.Class (throwError, catchError)
import Control.Parallel.Class (sequential, parallel)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.List (range)
import Data.Maybe (Maybe(..), maybe, isNothing)
import Node.Path (FilePath)
import Node.Process as P
import Node.Yargs.Applicative (flag, yarg, runY)
import Node.Yargs.Setup (usage, defaultHelp, defaultVersion)
import PscIde.Command (RebuildResult(RebuildResult))
import PscIde.Server (stopServer)

import PscPane.Config (Config)
import PscPane.Parser (PscResult(PscResult))
import PscPane.State (State(..), Progress(InProgress, Done), PaneResult(Warning, Error))
import PscPane.Server (startPscIdeServer)
import PscPane.Types (EffN, AffN)
import PscPane.Watcher (watch)
import Prelude hiding (append)

foreign import minimatch ∷ String → String → Boolean

buildProject ∷ A.Action Unit
buildProject = do
  err ← A.buildProject
  A.loadModules
  case firstFailure err of
    Just res →
      A.drawPaneState (PscError res)
    Nothing → do
      shouldRunTests ← A.shouldRunTests
      if shouldRunTests then
        do
          A.drawPaneState (BuildSuccess (InProgress "running tests..."))
          testResult ← A.runTests
          case testResult of
            Nothing → A.drawPaneState TestSuccess
            Just out → A.drawPaneState (TestFailure out)
        else 
          A.drawPaneState (BuildSuccess Done)
        
  pure unit

  where

  firstFailure ∷ PscResult → Maybe PaneResult
  firstFailure (PscResult { warnings: [], errors: [] }) = Nothing
  firstFailure (PscResult { warnings: [], errors: errors }) = Error <$> head errors
  firstFailure (PscResult { warnings: warnings, errors: [] }) = Warning <$> head warnings
  firstFailure (PscResult { warnings: _, errors: errors }) = Error <$> head errors

initialBuild ∷ A.Action Unit
initialBuild = do
  A.drawPaneState InitialBuild
  buildProject

rebuildModule ∷ String → A.Action Unit
rebuildModule path = do
  errors ← A.rebuildModule path
  let firstErr = takeOne errors
  A.drawPaneState (toPaneState path firstErr)
  when (isNothing firstErr) buildProject
  pure unit
    where
    takeOne ∷ Either RebuildResult RebuildResult → Maybe PaneResult
    takeOne (Right (RebuildResult warnings)) = Warning <$> head warnings
    takeOne (Left (RebuildResult errors)) = Error <$> head errors

    toPaneState ∷ FilePath → Maybe PaneResult → State
    toPaneState _ (Just res) = PscError res
    toPaneState path Nothing = ModuleOk path (InProgress "building project...")

app ∷ String → String → String → String → Boolean → Boolean → EffN Unit
app srcPath libPath testPath testMain test noColor = void do
  cwd ← P.cwd
  let
    screen = mkScreen { smartCSR: true }
    box = mkBox { width: "100%", height: "100%", content: "" }

    fail ∷ Error → EffN Unit
    fail err =
      let msg = "Error: " <> message err <> " (type q to quit)"
      in setContent box msg *> render screen

  append screen box
  render screen

  runAff fail pure do
    running ← startPscIdeServer cwd $ range 4242 4252
    port ← maybe (throwError (error "Cannot start psc-ide-server")) pure running
    let config ∷ Config
        config = { screen
                 , box
                 , port
                 , cwd
                 , srcPath
                 , libPath
                 , testPath
                 , testMain
                 , test
                 , prevPaneState: InitialBuild
                 , colorize: not noColor
                 }
    stateRef ← liftEff $ newRef config
    let
      runCmd ∷ A.Action Unit → AffN Unit
      runCmd program =
        let
          program' = do
            state ← liftEff $ readRef stateRef
            newState ← A.run state program
            liftEff $ writeRef stateRef newState
        in catchError program' (liftEff <<< fail)

      fileListener ∷ Consumer String AffN Unit
      fileListener = consumer \path → do
        when (any (minimatch path) ["**/*.purs"]) $ runCmd (rebuildModule path)

        -- Changing a .js file triggers a full build for now. Should we just
        -- build and load the purs file?
        when (any (minimatch path) ["**/*.js"]) $ runCmd buildProject

        pure Nothing

      handleResize ∷ Consumer Unit AffN Unit
      handleResize = consumer \_ → do
        { prevPaneState } ← liftEff $ readRef stateRef
        runCmd (A.drawPaneState prevPaneState)
        pure Nothing

      handleQuit ∷ Consumer Unit AffN Unit
      handleQuit = consumer \_ → do
        stopServer port
        liftEff $ P.exit 0
        pure Nothing

      resizeP ∷ AffN Unit
      resizeP = runProcess (onResize screen $$ handleResize)

      watchP ∷ AffN Unit
      watchP = let watchDirs = if test then [srcPath, testPath] else [srcPath]
               in runProcess (watch watchDirs $$ fileListener)

      quitP ∷ AffN Unit
      quitP = runProcess (onQuit screen ["q", "C-c"] $$ handleQuit)

    sequential
      $ parallel (runCmd initialBuild) <|> parallel resizeP <|> parallel watchP <|> parallel quitP

main ∷ EffN Unit
main = do
  let setup = usage "psc-pane - Auto reloading PureScript compiler\n\nUsage: psc-pane [OPTION]"
              <> defaultHelp
              <> defaultVersion
  runY setup $
    app
    <$> yarg "src" []
        (Just "Path to .purs sources. Default: \"src\".")
        (Left "src")
        true
    <*> yarg "lib" []
        (Just "Path to dependency files. Default: \"bower_components\"")
        (Left "bower_components")
        true
    <*> yarg "test-src" []
        (Just "Path to test sources. Default \"test\"")
        (Left "test")
        true
    <*> yarg "test-main" []
        (Just "Module with main function to run for tests. Default: \"Test.Main\"")
        (Left "Test.Main")
        true
    <*> flag "t" ["test"] (Just "Run tests after a successful build.")
    <*> flag "nocolor" [] (Just "Do not colorize output.")
