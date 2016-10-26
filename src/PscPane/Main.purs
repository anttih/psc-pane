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
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foldable (any, fold)
import Data.List (range)
import Data.Newtype (unwrap)
import Data.Maybe (Maybe(..), maybe, isNothing)
import Data.Maybe.First (First(..))
import Data.String (Pattern(..), split, trim)
import Node.Path (FilePath)
import Node.Process as P
import Node.Yargs.Applicative (flag, yarg, runY)
import Node.Yargs.Setup (usage, defaultHelp, defaultVersion)
import PscIde.Command (RebuildResult(RebuildResult))
import PscIde.Server (stopServer)
import PscPane.Parser (PscResult(PscResult))
import PscPane.Pretty (PaneState(InitialBuild, BuildSuccess, ModuleOk, PscError), PaneResult(Warning, Error))
import PscPane.Server (startPscIdeServer)
import PscPane.Types (EffN, AffN)
import PscPane.Watcher (watch)
import Prelude hiding (append)

foreign import minimatch ∷ String → String → Boolean

readErr ∷ String → Maybe PscResult
readErr err = findFirst jsonOutput lines
  where
  lines ∷ Array String
  lines = split (Pattern "\n") $ trim err

  jsonOutput ∷ String → Maybe PscResult
  jsonOutput line = eitherToMaybe do
    json ← jsonParser line
    decodeJson json

  eitherToMaybe ∷ forall e a. Either e a → Maybe a
  eitherToMaybe (Right a) = Just a
  eitherToMaybe _ = Nothing

  findFirst ∷ (String → Maybe PscResult) → Array String → Maybe PscResult
  findFirst f xs = unwrap (fold (map (First <<< f) xs))

buildProject ∷ A.Action Unit
buildProject = do
  err ← A.buildProject
  A.loadModules
  maybe (A.showError err)
        (A.drawPaneState <<< toPaneState <<< firstFailure)
        (readErr err)
  pure unit
    where
    firstFailure ∷ PscResult → Maybe PaneResult
    firstFailure (PscResult { warnings: [], errors: [] }) = Nothing
    firstFailure (PscResult { warnings: [], errors: errors }) = Error <$> head errors
    firstFailure (PscResult { warnings: warnings, errors: [] }) = Warning <$> head warnings
    firstFailure (PscResult { warnings: _, errors: errors }) = Error <$> head errors

    toPaneState ∷ Maybe PaneResult → PaneState
    toPaneState = maybe BuildSuccess PscError

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

    toPaneState ∷ FilePath → Maybe PaneResult → PaneState
    toPaneState _ (Just res) = PscError res
    toPaneState path Nothing = ModuleOk path "building project..."

app ∷ String → Array String → Boolean → EffN Unit
app buildCmd dirs noColor = void do
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
    stateRef ← liftEff $ newRef { screen
                                , box
                                , port
                                , cwd
                                , buildCmd
                                , prevPaneState: InitialBuild
                                , colorize: not noColor
                                }
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
      watchP = runProcess (watch dirs $$ fileListener)

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
    <$> yarg "c" ["command"]
        (Just ("Build command. Should return JSON in stderr. "
          <> "Default:  psc 'src/**/*.purs' 'bower_components/purescript-*/src/**/*.purs' --json-errors"))
        (Left "psc 'src/**/*.purs' 'bower_components/purescript-*/src/**/*.purs' --json-errors")
        true
    <*> yarg "w" ["watch-path"]
        (Just  "Directory to watch for changes (default: \"src\")")
        (Left ["src"])
        true
    <*> flag "nocolor" [] (Just "Do not colorize output.")
