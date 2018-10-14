module PscPane.Main where

import Prelude hiding (append)

import Blessed (onResize, onQuit, render, append, setContent, mkBox, mkScreen, destroy)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError, catchError)
import Data.Either (Either(..))
import Data.List ((..))
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff, runAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error, error, message)
import Effect.Ref as Ref
import Node.Process as P
import Node.Yargs.Applicative (flag, yarg, runY)
import Node.Yargs.Setup (usage, defaultHelp, defaultVersion)
import PscPane.Config (Options)
import PscPane.Interpreter (run)
import PscPane.Program (ACTION, Event(..), ExitReason, eval)
import PscPane.Server (startPscIdeServer)
import PscPane.State (State(..))
import PscPane.Watcher (onFileChange)
import Run (AFF, EFFECT, Run)
import Run.Except (EXCEPT)
import Stream (Stream, emit, subscribe)

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
    running ← startPscIdeServer cwd $ 4242 .. 4252
    port ← maybe (throwError (error "Cannot start psc-ide-server")) pure running
    let config = { screen, box, port, cwd, prevPaneState: InitialBuild, options }
    stateRef ← liftEffect $ Ref.new config
    let
      watchDirs = if test then [srcPath, testPath] else [srcPath]

      runDSL ∷ Run (action :: ACTION, except :: EXCEPT ExitReason, effect :: EFFECT, aff :: AFF) Unit → Aff Unit
      runDSL program = catchError (run stateRef program) (liftEffect <<< showError)

      events :: Stream Event
      events
        = emit Init
        <|> (Quit <$ onQuit screen ["q", "C-c"])
        <|> (Resize <$ onResize screen)
        <|> (FileChange <$> onFileChange watchDirs)

    subscribe events (runDSL <<< eval)

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
