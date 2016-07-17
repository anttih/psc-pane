module PscPane.Main where

import Prelude hiding (append)
import Control.Apply ((*>))
import Control.Monad.Aff (runAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Data.List (range)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foldable (any, fold)
import Data.Maybe (Maybe(..), maybe, isNothing)
import Data.Maybe.First (First(..), runFirst)
import Data.String (split, trim)
import Node.Path (FilePath)
import Node.Process (cwd, exit) as P
import Node.Yargs.Applicative (yarg, runY)
import Node.Yargs.Setup (usage, defaultHelp, defaultVersion)
import Blessed (append, render, mkBox, mkScreen, on)

import PscIde.Command (RebuildResult(RebuildResult))

import PscPane.Parser (PscResult(PscResult))
import PscPane.Pretty (PaneState(InitialBuild, BuildSuccess, ModuleOk, PscError), PaneResult(Warning, Error))
import PscPane.Server (startPscIdeServer)
import PscPane.Types (EffN, AffN)
import PscPane.Watcher (watch)
import PscPane.Action as A

foreign import minimatch :: String -> String -> Boolean

readErr :: String -> Maybe PscResult
readErr err = findFirst jsonOutput lines
  where
  lines :: Array String
  lines = split "\n" $ trim err

  jsonOutput :: String -> Maybe PscResult
  jsonOutput line = eitherToMaybe do
    json <- jsonParser line
    decodeJson json

  eitherToMaybe :: forall e a. Either e a -> Maybe a
  eitherToMaybe (Right a) = Just a
  eitherToMaybe _ = Nothing

  findFirst :: (String -> Maybe PscResult) -> Array String -> Maybe PscResult
  findFirst f xs = runFirst (fold (map (First <<< f) xs))

buildProject :: A.Action Unit
buildProject = do
  err ← A.buildProject
  A.loadModules
  maybe (A.showError err)
        (A.drawPaneState <<< toPaneState <<< firstFailure)
        (readErr err)
  pure unit
    where
    firstFailure :: PscResult -> Maybe PaneResult
    firstFailure (PscResult { warnings: [], errors: [] }) = Nothing
    firstFailure (PscResult { warnings: [], errors: errors }) = Error <$> head errors
    firstFailure (PscResult { warnings: warnings, errors: [] }) = Warning <$> head warnings
    firstFailure (PscResult { warnings: _, errors: errors }) = Error <$> head errors

    toPaneState :: Maybe PaneResult → PaneState
    toPaneState = maybe BuildSuccess PscError

initialBuild :: A.Action Unit
initialBuild = do
  A.drawPaneState InitialBuild
  buildProject

rebuildModule :: String -> A.Action Unit
rebuildModule path = do
  errors <- A.rebuildModule path
  let firstErr = takeOne errors
  A.drawPaneState (toPaneState path firstErr)
  when (isNothing firstErr) buildProject
  pure unit
    where
    takeOne :: Either RebuildResult RebuildResult -> Maybe PaneResult
    takeOne (Right (RebuildResult warnings)) = Warning <$> head warnings
    takeOne (Left (RebuildResult errors)) = Error <$> head errors

    toPaneState :: FilePath -> Maybe PaneResult -> PaneState
    toPaneState _ (Just res) = PscError res
    toPaneState path Nothing = ModuleOk path "building project..."

app :: String -> Array String -> EffN Unit
app buildCmd dirs = void $ runAff logShow pure do
  cwd <- liftEff P.cwd
  running <- startPscIdeServer cwd $ range 4242 4252
  let screen = mkScreen { smartCSR: true }
  let box = mkBox { width: "100%", height: "100%", content: "" }
  liftEff $ append screen box
  liftEff $ render screen
  maybe quit (\port -> do
    stateRef ← liftEff $ newRef { screen, box, port, cwd, buildCmd, prevPaneState: InitialBuild }
    let runCmd program = do
          state ← liftEff $ readRef stateRef
          newState ← A.run state program
          liftEff $ writeRef stateRef newState
    runCmd initialBuild
    liftEff $ watch dirs \path -> do
      when (any (minimatch path) ["**/*.purs"]) $
        void $ runAff logShow pure (runCmd (rebuildModule path))

      -- Changing a .js file triggers a full build for now. Should we just
      -- build and load the purs file?
      when (any (minimatch path) ["**/*.js"]) $
        void $ runAff logShow pure (runCmd buildProject)

    liftEff $ on screen "resize" \_ -> do
      { prevPaneState } ← readRef stateRef
      void $ runAff logShow pure (runCmd (A.drawPaneState prevPaneState))
  ) running
  where
    quit :: AffN Unit
    quit = do
      log "Cannot start psc-ide-server"
      liftEff (P.exit 1)

main :: EffN Unit
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
