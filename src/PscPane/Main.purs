module PscPane.Main where

import Prelude hiding (append)
import Control.Alt ((<|>))
import Control.Coroutine (Consumer, runProcess, consumer, ($$))
import Control.Monad.Aff (runAff, attempt)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception (Error, error, message)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.Error.Class (throwError, catchError)
import Control.Parallel.Class (sequential, parallel)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.List (range)
import Data.Maybe (Maybe(..), maybe, isNothing)
import Node.Process as P
import Node.Yargs.Applicative (flag, yarg, runY)
import Node.Yargs.Setup (usage, defaultHelp, defaultVersion)
import PscIde.Server (stopServer)

import Blessed (onResize, onQuit, render, append, setContent, mkBox, mkScreen,
               destroy, debug)
import PscPane.DSL as A
import PscPane.Interpreter (run)
import PscPane.State (State(..), Progress(InProgress, Done), PscFailure)
import PscPane.Server (startPscIdeServer)
import PscPane.Types (EffN, AffN)
import PscPane.Watcher (watch)
import PscPane.Config (Options)

foreign import minimatch ∷ String → String → Boolean

buildProject ∷ A.Action Unit
buildProject = do
  err ← A.buildProject
  A.loadModules
  case err of
    Just res →
      A.drawPaneState (PscError res)
    Nothing → do
      shouldRunTests ← A.shouldRunTests
      if shouldRunTests then
        do
          A.drawPaneState (BuildSuccess (InProgress "running tests..."))
          testResult ← A.runTests
          case testResult of
            Left out → A.drawPaneState (TestFailure out)
            Right _ → A.drawPaneState TestSuccess
        else 
          A.drawPaneState (BuildSuccess Done)
        
  pure unit

initialBuild ∷ A.Action Unit
initialBuild = do
  A.drawPaneState InitialBuild
  buildProject

rebuildModule ∷ String → A.Action Unit
rebuildModule path = do
  A.drawPaneState (CompilingModule path)
  firstErr ← A.rebuildModule path
  rebuild ← A.shouldBuildAll
  A.drawPaneState (toPaneState firstErr rebuild)
  when (isNothing firstErr && rebuild) buildProject
  pure unit

  where
  toPaneState ∷ Maybe PscFailure → Boolean → State
  toPaneState (Just res) _ = PscError res
  toPaneState Nothing true = ModuleOk path (InProgress "building project...")
  toPaneState Nothing false = ModuleOk path Done

app ∷ Options → EffN Unit
app options@{ srcPath, testPath, test } = void do
  cwd ← P.cwd
  let
    screen = mkScreen { smartCSR: true, debug: false }
    box = mkBox { width: "100%"
                , height: "100%"
                , content: ""
                , scrollable: true
                , scrollbar: true
                , keys: true
                , alwaysScroll: true
                , vi: true }

    exit ∷ Error → EffN Unit
    exit err = do
      destroy screen
      Console.error $ "Error: " <> message err
      P.exit (-1)

    showError ∷ Error → EffN Unit
    showError err =
      let msg = "Error: " <> message err <> " (type q to quit)"
      in setContent box msg *> render screen

  append screen box
  render screen

  runAff exit pure do
    running ← startPscIdeServer cwd $ range 4242 4252
    port ← maybe (throwError (error "Cannot start psc-ide-server")) pure running
    let config = { screen, box, port, cwd, prevPaneState: InitialBuild, options }
    stateRef ← liftEff $ newRef config
    let
      runCmd ∷ A.Action Unit → AffN Unit
      runCmd program =
        let
          program' = do
            state ← liftEff $ readRef stateRef
            newState ← run state program
            liftEff $ writeRef stateRef newState
        in catchError program' (liftEff <<< showError)

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
        attempt $ stopServer port
        liftEff $ P.exit 0
        pure Nothing

      resizeP ∷ AffN Unit
      resizeP = runProcess (onResize screen $$ handleResize)

      watchP ∷ AffN Unit
      watchP = let watchDirs = if test then [srcPath, testPath] else [srcPath]
               in runProcess (watch watchDirs $$ fileListener)

      quitP ∷ AffN Unit
      quitP = runProcess (onQuit screen ["q", "C-c"] $$ handleQuit)

    -- Wait for the initial build to finish first
    runCmd initialBuild

    -- Run event listeners in parallel. They don't ever finish.
    sequential $ parallel resizeP <|> parallel watchP <|> parallel quitP

main ∷ EffN Unit
main = do
  let setup = usage "psc-pane - Auto reloading PureScript compiler\n\nUsage: psc-pane [OPTION]"
              <> defaultHelp
              <> defaultVersion
      options = { buildPath: _, srcPath: _, libPath: _, testPath: _, testMain: _
                , rebuild: _, test: _, colorize: _}
  runY setup $
    map app $
      options
      <$> yarg "o" ["build-path" ]
          (Just "Directory for psc output (default \"output\")")
          (Left "output")
          true
      <*> yarg "src-path" []
          (Just "Directory for .purs source files (default: \"src\")")
          (Left "src")
          true
      <*> yarg "dependency-path" []
          (Just "Directory for dependencies (default: \"bower_components\")")
          (Left "bower_components")
          true
      <*> yarg "test-path" []
          (Just "Directory for .purs test source files (default: \"test\")")
          (Left "test")
          true
      <*> yarg "test-main" []
          (Just "Module with main function for running tests (default: \"Test.Main\")")
          (Left "Test.Main")
          true
      <*> (not <$> flag "m" ["norebuild"]
          (Just "Single module mode. Only use psc-ide to compile one module at a time."))
      <*> flag "t" ["test"] (Just "Run tests after a successful build")
      <*> (not <$> flag "nocolor" [] (Just "Do not colorize output"))
