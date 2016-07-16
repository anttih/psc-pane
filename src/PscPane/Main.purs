module PscPane.Main where

import Prelude
import Control.Apply ((*>))
import Control.Monad.Aff (makeAff, runAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Class (liftEff)
import Data.List (range)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (head)
import Data.Either (Either(..), either)
import Data.Foldable (any, fold)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..), maybe, isNothing)
import Data.Maybe.First (First(..), runFirst)
import Data.String (split, trim)
import Node.Buffer (Buffer, toString)
import Node.Encoding (Encoding(UTF8))
import Node.Path (FilePath)
import Node.Process (cwd, exit)
import Node.Yargs.Applicative (yarg, runY)
import Node.Yargs.Setup (usage, defaultHelp, defaultVersion)
import PscIde (load, listLoadedModules, rebuild)
import PscIde.Command (RebuildResult(RebuildResult))

import PscPane.Output (clear, display, write)
import PscPane.Parser (PscResult(PscResult))
import PscPane.Pretty (PaneState(BuildSuccess, ModuleOk, PscError), PaneResult(Warning, Error), formatState)
import PscPane.Server (startPscIdeServer)
import PscPane.Types (EffN, AffN)
import PscPane.Watcher (watch)

foreign import minimatch :: String -> String -> Boolean

foreign import spawn :: Fn2 String (Buffer -> EffN Unit) (EffN Unit)

spawnAff :: String -> AffN Buffer
spawnAff cmd = makeAff (\error success -> runFn2 spawn cmd success)

loadModules :: Int -> AffN Unit
loadModules port = do
  loaded <- load port [] []
  void $ listLoadedModules port

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

runBuildCmd :: Int -> String -> String -> AffN Unit
runBuildCmd port dir cmd = do
  buf <- spawnAff cmd
  height <- liftEff rows
  err <- liftEff $ toString UTF8 buf
  loadModules port
  maybe (display err)
        (display <<< formatState dir height <<< toPaneState <<< firstFailure)
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

rebuildModule :: Int -> String -> String -> String -> AffN Unit
rebuildModule port dir cmd path = do
  height <- liftEff rows
  res <- rebuild port path
  clear
  either (liftEff <<< write) (\errors -> do
    let firstErr = takeOne errors
    liftEff $ write (formatState dir height (toPaneState path firstErr))
    when (isNothing firstErr) (runBuildCmd port dir cmd)
  ) res
  pure unit
    where
    takeOne :: Either RebuildResult RebuildResult -> Maybe PaneResult
    takeOne (Right (RebuildResult warnings)) = Warning <$> head warnings
    takeOne (Left (RebuildResult errors)) = Error <$> head errors

    toPaneState :: FilePath -> Maybe PaneResult -> PaneState
    toPaneState _ (Just res) = PscError res
    toPaneState path Nothing = ModuleOk path "building project..."

foreign import rows :: EffN Int

build :: Int -> String -> String -> AffN Unit
build port dir cmd = do
  display "Building project..."
  runBuildCmd port dir cmd

app :: String -> Array String -> EffN Unit
app cmd dirs = void $ runAff logShow pure do
  dir <- liftEff cwd
  running <- startPscIdeServer dir $ range 4242 4252
  maybe quit (\port -> do
    build port dir cmd
    liftEff $ watch dirs \path -> do
      when (any (minimatch path) ["**/*.purs"]) $
        void $ runAff logShow pure (rebuildModule port dir cmd path)

      -- Changing a .js file triggers a full build for now. Should we just
      -- build and load the purs file?
      when (any (minimatch path) ["**/*.js"]) $
        void $ runAff logShow pure (build port dir cmd)
  ) running
  where
    quit :: AffN Unit
    quit = do
      log "Cannot start psc-ide-server"
      liftEff (exit 1)

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
