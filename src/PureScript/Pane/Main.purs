module PureScript.Pane.Main where

import Prelude
import Control.Apply ((*>))
import Control.Monad (unless, when)
import Control.Monad.Aff (makeAff, launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (head, length)
import Data.Either (Either(..), either)
import Data.Foldable (any, fold)
import Data.Function (Fn2, runFn2)
import Data.Maybe (isNothing, Maybe(..), maybe)
import Data.Maybe.First (First(..), runFirst)
import Data.String (split, trim)
import Node.Buffer (Buffer, toString)
import Node.Encoding (Encoding(UTF8))
import Node.Path (FilePath)
import Node.Process (cwd, exit)
import Node.Yargs.Applicative (yarg, runY)
import Node.Yargs.Setup (usage, help)
import PscIde (load, listLoadedModules, rebuild)
import PscIde.Command (ModuleList(ModuleList), RebuildResult(RebuildResult))
import PureScript.Pane.Color (green)
import PureScript.Pane.Parser (PscResult(PscResult))
import PureScript.Pane.Pretty (Height, PaneResult(Warning, Error), pretty)
import PureScript.Pane.Server (startServer, serverRunning)
import PureScript.Pane.Types (EffN, AffN)

foreign import watch :: Array String -> (String -> EffN Unit) -> EffN Unit

watchAff :: Array String -> (String -> AffN Unit) -> AffN Unit
watchAff dirs callback =
  liftEff (watch dirs (\path -> launchAff (callback path)))

foreign import minimatch :: String -> String -> Boolean

foreign import spawn :: Fn2 String (Buffer -> EffN Unit) (EffN Unit)

spawnAff :: String -> AffN Buffer
spawnAff cmd = makeAff (\error success -> runFn2 spawn cmd success)

foreign import write :: String -> EffN Unit

clear :: AffN Unit
clear = do
  liftEff (write "\x1b[2J")
  liftEff (write "\x1b[1;1H")
  pure unit

loadModules :: Int -> AffN Int
loadModules port = do
  loaded <- load port [] []
  modules <- listLoadedModules port
  pure $ either (const 0) (\(ModuleList xs) -> length xs) (loaded *> modules)

readErr :: String -> Maybe PscResult
readErr err =
  let lines = split "\n" (trim err)
  in findFirst jsonOutput lines

    where
    jsonOutput :: String -> Maybe PscResult
    jsonOutput line = eitherToMaybe do
      json <- jsonParser line
      decodeJson json

    eitherToMaybe :: forall e a. Either e a -> Maybe a
    eitherToMaybe (Right a) = Just a
    eitherToMaybe _ = Nothing

    findFirst :: (String -> Maybe PscResult) -> Array String -> Maybe PscResult
    findFirst f xs = runFirst (fold (map (First <<< f) xs))

compileAll :: Int -> String -> AffN Unit
compileAll port cmd = do
  buf <- spawnAff cmd
  height <- liftEff rows
  dir <- liftEff cwd
  err <- liftEff (toString UTF8 buf)
  mods <- loadModules port
  clear
  liftEff $ write (maybe err (showResult dir height mods <<< takeOne) (readErr err))
  pure unit
    where
      takeOne :: PscResult -> Maybe PaneResult
      takeOne (PscResult { warnings: [], errors: [] }) = Nothing
      takeOne (PscResult { warnings: [], errors: errors }) = Error <$> head errors
      takeOne (PscResult { warnings: warnings, errors: [] }) = Warning <$> head warnings
      takeOne (PscResult _) = Nothing

      showResult :: FilePath -> Height -> Int -> Maybe PaneResult -> String
      showResult dir height _ (Just res) = pretty dir height res
      showResult _ _ mods Nothing = green "Build successful" <> " (loaded " <> (show mods) <> " modules)"

recompile :: Int -> String -> String -> AffN Unit
recompile port cmd path = do
  clear
  height <- liftEff rows
  dir <- liftEff cwd
  res <- rebuild port path
  either (liftEff <<< write) (\res' -> do
    let res'' = takeOne res'
    liftEff $ write (showResult dir height res'')
    when (isNothing res'') (compileAll port cmd)
  ) res
  pure unit
  where
    takeOne :: Either RebuildResult RebuildResult -> Maybe PaneResult
    takeOne (Right (RebuildResult warnings)) = Warning <$> head warnings
    takeOne (Left (RebuildResult errors)) = Error <$> head errors

    showResult :: FilePath -> Height -> Maybe PaneResult -> String
    showResult dir height (Just res) = pretty dir height res
    showResult _ _ Nothing = green "Module OK" <> " " <> path <> " (building project...)"

foreign import rows :: EffN Int

app :: String -> Array String -> EffN Unit
app cmd dirs = launchAff do
  let port = 4040
  running <- serverRunning <$> startServer "psc-ide-server" port
  unless running do
    log "Cannot start psc-ide-server"
    liftEff (exit 1)
  clear
  log "Building project..."
  compileAll port cmd
  watchAff dirs \path -> do
    when (any (minimatch path) ["**/*.purs", "**/*.js"]) (recompile port cmd path)

main :: EffN Unit
main = do
  let setup = usage "psc-pane - Auto reloading PureScript compiler\n\nUsage: psc-pane [OPTION]"
              <> help "help" "Show this help"
  runY setup $
    app
    <$> yarg "c" ["command"]
        (Just "Build command. Should return JSON in stderr. Default: pulp build --no-psa --json-errors")
        (Left "pulp build --no-psa --json-errors")
        true
    <*> yarg "w" ["watch-path"]
      (Just  "Directory to watch for changes (default: \"src\")")
      (Left ["src"])
      true
