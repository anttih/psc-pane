module PureScript.Pane.Main where

import Prelude ((<*>), (<<<), (<$>), ($), (<>), Unit, bind, unit, pure, const, map)
import Control.Monad (when)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Aff (Aff(), makeAff, launchAff)
import Control.Monad.Aff.Console (log)
import Data.Foldable (any, fold)
import Data.Foreign.Class (readJSON)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe.First (First(..), runFirst)
import Data.Function (Fn2(), runFn2)
import Data.String (split, trim)

import Node.FS (FS())
import Node.Buffer (Buffer(), BUFFER(), toString)
import Node.Encoding (Encoding(UTF8))
import Node.Process (PROCESS, cwd)

import Node.Yargs.Setup (usage, help)
import Node.Yargs.Applicative (yarg, runY)

import PureScript.Pane.Pretty (pretty)
import PureScript.Pane.Parser (PscResult())

type EffN = Eff ( fs :: FS
                , err :: EXCEPTION
                , console :: CONSOLE
                , buffer :: BUFFER
                , process :: PROCESS)

type AffN = Aff ( fs :: FS
                , err :: EXCEPTION
                , console :: CONSOLE
                , process :: PROCESS
                , buffer :: BUFFER)

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

compile :: String -> AffN Unit
compile cmd = do
  clear
  log "Compiling..."
  buf <- spawnAff cmd
  height <- liftEff rows
  clear
  dir <- liftEff cwd
  err <- liftEff (toString UTF8 buf)
  liftEff $ write (maybe err (pretty dir height) (readErr err))
  pure unit

foreign import rows :: EffN Int

app :: String -> Array String -> EffN Unit
app cmd dirs = launchAff do
  compile cmd
  watchAff dirs \path -> do
    when (any (minimatch path) ["**/*.purs", "**/*.js"]) (compile cmd)

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
