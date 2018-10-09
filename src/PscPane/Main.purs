module PscPane.Main where

import Prelude hiding (append)

import Blessed (onResize, onQuit, render, append, setContent, mkBox, mkScreen, destroy)
import Control.Coroutine (Consumer, Producer, consumer, runProcess, transform, ($$), ($~))
import Control.Coroutine.Aff (close, emit, produceAff)
import Control.Monad.Error.Class (throwError, catchError)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.List (range)
import Data.Maybe (Maybe(..), maybe, isNothing)
import Effect (Effect)
import Effect.Aff (Aff, forkAff, parallel, runAff, sequential)
import Effect.Aff.AVar as AV
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error, error, message)
import Effect.Ref as Ref
import Node.Process as P
import Node.Yargs.Applicative (flag, yarg, runY)
import Node.Yargs.Setup (usage, defaultHelp, defaultVersion)
import PscPane.Config (Options)
import PscPane.DSL (ask, exit)
import PscPane.DSL as A
import PscPane.Interpreter (run)
import PscPane.Server (startPscIdeServer)
import PscPane.State (State(..), Progress(InProgress, Done), PscFailure)
import PscPane.Watcher (watch)

foreign import minimatch ∷ String → String → Boolean

data Query = Resize | Quit | FileChange String

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

app ∷ Options → Effect Unit
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

    handleAff :: Either Error Unit -> Effect Unit
    handleAff = case _ of
      Left err -> exit err
      Right _ -> pure unit

      where
      exit ∷ Error → Effect Unit
      exit err = do
        destroy screen
        Console.error $ "Error: " <> message err
        P.exit (-1)

    showError ∷ Error → Effect Unit
    showError err =
      let msg = "Error: " <> message err <> " (type q to quit)"
      in setContent box msg *> render screen

  append screen box
  render screen

  runAff handleAff do
    running ← startPscIdeServer cwd $ range 4242 4252
    port ← maybe (throwError (error "Cannot start psc-ide-server")) pure running
    let config = { screen, box, port, cwd, prevPaneState: InitialBuild, options }
    stateRef ← liftEffect $ Ref.new config
    let
      watchDirs = if test then [srcPath, testPath] else [srcPath]

      runCmd ∷ A.Action Unit → Aff Unit
      runCmd program =
        let
          program' = do
            state ← liftEffect $ Ref.read stateRef
            newState ← run state program
            liftEffect $ Ref.write newState stateRef
        in catchError program' (liftEffect <<< showError)

      run' :: Query -> A.Action Unit
      run' = case _ of
        Quit -> exit
        Resize -> do
          { prevPaneState } <- ask
          A.drawPaneState prevPaneState
        FileChange path -> do
          when (any (minimatch path) ["**/*.purs"]) (rebuildModule path)
          -- Changing a .js file triggers a full build for now. Should we just
          -- build and load the purs file?
          when (any (minimatch path) ["**/*.js"]) buildProject

      handle :: Consumer Query Aff Unit
      handle = consumer \q -> runCmd (run' q) *> pure Nothing

      queryProducer :: Producer Query Aff Unit
      queryProducer =
        (onQuit screen ["q", "C-c"] $~ transform (const Quit))
        `mergeProducers`
        (onResize screen $~ forever (transform (const Resize)))
        `mergeProducers`
        (watch watchDirs $~ forever (transform FileChange))

    -- Wait for the initial build to finish first
    runCmd initialBuild

    -- Run event listeners in parallel. They don't ever finish.
    runProcess (queryProducer $$ handle)

mergeProducers :: forall o a. Producer o Aff a -> Producer o Aff a -> Producer o Aff Unit
mergeProducers l r = do
  var <- lift AV.empty

  let c = consumer \i -> AV.put i var *> pure Nothing

  void $ lift $ forkAff do
    void $ sequential $ parallel (runProcess (l $$ c)) *> parallel (runProcess (r $$ c))
    AV.kill (error "Both ended") var

  produceAff \emitter -> do
    let go = do
          status <- AV.status var
          if AV.isKilled status
            then close emitter unit
            else do
              a <- AV.take var
              emit emitter a
              go
    go

main ∷ Effect Unit
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
          (Just "Directory for `purs compile` output (default \"output\")")
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
