module PureScript.Pane.Main where

import Prelude
import Control.Monad (when)
import Control.Monad.Aff (makeAff, launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (Either(..), either)
import Data.Foldable (any, fold)
import Data.Foreign.Class (readJSON)
import Data.Function (Fn2, runFn2)
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe.First (First(..), runFirst)
import Data.String (split, trim)
import Node.Buffer (Buffer, toString)
import Node.Encoding (Encoding(UTF8))
import Node.Process (cwd)
import Node.Yargs.Applicative (yarg, runY)
import Node.Yargs.Setup (usage, help)
import PscIde (load, rebuild)
import PscIde.Command (RebuildError(RebuildError), RebuildResult(RebuildResult))
import PureScript.Pane.Parser (Position(Position), PscError(PscError), PscResult(PscResult))
import PureScript.Pane.Pretty (pretty)
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

readErr :: String -> Maybe PscResult
readErr err = findFirst jsonOutput lines
  where lines = split "\n" (trim err)
        jsonOutput line = either (const Nothing) Just (readJSON line)

        findFirst :: (String -> Maybe PscResult) -> Array String -> Maybe PscResult
        findFirst f xs = runFirst (fold (map (First <<< f) xs))

compileAll :: String -> AffN Unit
compileAll cmd = do
  clear
  log "Compiling..."
  buf <- spawnAff cmd
  height <- liftEff rows
  clear
  dir <- liftEff cwd
  err <- liftEff (toString UTF8 buf)
  liftEff $ write (maybe err (pretty dir height) (readErr err))
  pure unit

recompile :: String -> AffN Unit
recompile path = do
  clear
  height <- liftEff rows
  dir <- liftEff cwd
  res <- rebuild 4040 path
  liftEff $ either write (write <<< pretty dir height <<< showErrors) res
  pure unit
  where
    showErrors :: Either RebuildResult RebuildResult -> PscResult
    showErrors (Right (RebuildResult warnings)) = PscResult { warnings: toPscError <$> warnings
                                                            , errors: []
                                                            }
    showErrors (Left (RebuildResult errors)) = PscResult { warnings: []
                                                         , errors: toPscError <$> errors
                                                         }

    toPscError :: RebuildError -> PscError
    toPscError (RebuildError err) = PscError
                      { moduleName: err.moduleName
                      , filename: err.filename
                      , errorCode: err.errorCode
                      , message: split "\n" err.message
                      , position: (\{ line, column } -> Position
                          { startLine: line
                          , endLine: 0
                          , startColumn: column
                          , endColumn: 0
                        }) <$> err.position
                      }



foreign import rows :: EffN Int

app :: String -> Array String -> EffN Unit
app cmd dirs = launchAff do
  compileAll cmd
  running <- serverRunning <$> startServer "psc-ide-server" 4040
  if running
    then do
      res <- load 4040 [] []
      either log (const (pure unit)) res
    else log "Could not start psc-ide-server"
  watchAff dirs \path -> do
    when (any (minimatch path) ["**/*.purs", "**/*.js"]) (recompile path)

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
