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
import Data.Array (head, length)
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
import PscIde.Command (ModuleList(ModuleList), RebuildResult(RebuildResult))
import PscPane.Color (green, red)
import PscPane.Parser (PscResult(PscResult))
import PscPane.Pretty (Height, PaneResult(Warning, Error), pretty)
import PscPane.Server (startPscIdeServer)
import PscPane.Types (EffN, AffN)
import PscPane.Watcher (watch)
import PscPane.Test (runTests)

foreign import minimatch :: String -> String -> Boolean

foreign import spawn :: Fn2 String (Buffer -> EffN Unit) (EffN Unit)

spawnAff :: String -> AffN Buffer
spawnAff cmd = makeAff (\error success -> runFn2 spawn cmd success)

foreign import write :: String -> EffN Unit

clear :: AffN Unit
clear = do
  liftEff $ write "\x1b[2J"
  liftEff $ write "\x1b[1;1H"
  pure unit

loadModules :: Int -> AffN Int
loadModules port = do
  loaded <- load port [] []
  modules <- listLoadedModules port
  pure $ either (const 0) (\(ModuleList xs) -> length xs) $ loaded *> modules

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

type Progress = String

data PaneState = BuildSuccess
               | ModuleOk FilePath Progress
               | PscError PaneResult
               | TestFailure String

formatState :: FilePath → Height → PaneState → String
formatState cwd height (PscError res) = pretty cwd height res
formatState _ _ (TestFailure err) = red "Test failure" <> err
formatState _ _ (ModuleOk path progress) = green "Module OK" <> " " <> path <> " (" <> progress <> ")"
formatState _ _ BuildSuccess = green "Build successful"

display ∷ String → AffN Unit
display content = clear *> liftEff (write content)

runBuildCmd :: Int -> String -> String -> AffN Unit
runBuildCmd port dir cmd = do
  buf <- spawnAff cmd
  height <- liftEff rows
  err <- liftEff $ toString UTF8 buf
  mods <- loadModules port
  display $ formatState dir height (ModuleOk dir "running tests...")
  test <- runTests "pulp test --no-psa"
  maybe (display err)
        (display <<< formatState dir height <<< toPaneState test <<< firstFailure)
        (readErr err)
  pure unit
    where
    firstFailure :: PscResult -> Maybe PaneResult
    firstFailure (PscResult { warnings: [], errors: [] }) = Nothing
    firstFailure (PscResult { warnings: [], errors: errors }) = Error <$> head errors
    firstFailure (PscResult { warnings: warnings, errors: [] }) = Warning <$> head warnings
    firstFailure (PscResult { warnings: _, errors: errors }) = Error <$> head errors

    toPaneState :: Maybe String → Maybe PaneResult → PaneState
    toPaneState _ (Just res) = PscError res
    toPaneState Nothing Nothing = BuildSuccess
    toPaneState (Just err) Nothing = TestFailure err

rebuildModule :: Int -> String -> String -> String -> AffN Unit
rebuildModule port dir cmd path = do
  height <- liftEff rows
  res <- rebuild port path
  clear
  either (liftEff <<< write) (\errors -> do
    let firstErr = takeOne errors
    liftEff $ write (formatState dir height (toPaneState dir firstErr))
    when (isNothing firstErr) (runBuildCmd port dir cmd)
  ) res
  pure unit
    where
    takeOne :: Either RebuildResult RebuildResult -> Maybe PaneResult
    takeOne (Right (RebuildResult warnings)) = Warning <$> head warnings
    takeOne (Left (RebuildResult errors)) = Error <$> head errors

    toPaneState :: FilePath -> Maybe PaneResult -> PaneState
    toPaneState _ (Just res) = PscError res
    toPaneState dir Nothing = ModuleOk dir "building project..."

foreign import rows :: EffN Int

build :: Int -> String -> String -> AffN Unit
build port dir cmd = do
  clear
  log "Building project..."
  runBuildCmd port dir cmd

app :: String -> String -> Array String -> EffN Unit
app cmd testCmd dirs = void $ runAff logShow pure do
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
    <*> yarg "t" ["test"]
        (Just "Test command")
        (Left "pulp test")
        true 
    <*> yarg "w" ["watch-path"]
      (Just  "Directory to watch for changes (default: \"src\")")
      (Left ["src"])
      true
