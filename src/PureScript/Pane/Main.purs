module PureScript.Pane.Main where

import Prelude ((<*>), (<$>), ($), (<>), (>>=), Unit, bind, unit, pure, const, map)
import Control.Monad (when)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (Error(), EXCEPTION())
import Control.Monad.Aff (Aff(), makeAff, launchAff)
import Control.Monad.Aff.Console (log)
import Data.Array (concatMap, last)
import Data.Foldable (any)
import Data.Foreign.Class (readJSON)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Function (Fn4(), runFn4)
import Data.String (joinWith, split, trim)

import Node.FS (FS())
import Node.Buffer (Buffer(), BUFFER(), toString)
import Node.Encoding (Encoding(UTF8))
import Node.Process (PROCESS, cwd)

import Node.Yargs.Setup (example, usage, help)
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

foreign import spawn ::
  Fn4
  String
  (Array String)
  (Error -> EffN Unit)
  (Buffer -> EffN Unit)
  (EffN Unit)

spawnAff :: String -> Array String -> AffN Buffer
spawnAff cmd args = makeAff (\error success -> runFn4 spawn cmd args error success)

foreign import write :: String -> EffN Unit

clear :: AffN Unit
clear = do
  liftEff (write "\x1b[2J")
  liftEff (write "\x1b[1;1H")
  pure unit

readErr :: String -> Maybe PscResult
readErr err = let lines = split "\n" (trim err)
              in last lines >>= readLine
  where readLine line = either (const Nothing) Just (readJSON line)

compile :: Array String -> AffN Unit
compile args = do
  clear
  log "Compiling..."
  buf <- spawnAff "psc" args
  height <- liftEff rows
  clear
  dir <- liftEff cwd
  err <- liftEff (toString UTF8 buf)
  liftEff $ write (maybe err (pretty dir height) (readErr err))
  pure unit

foreign import rows :: EffN Int

buildArgs :: Array String -> String -> Array String -> Array String
buildArgs ffi out rest =
  concatMap (\f -> ["--ffi", f]) ffi
  <> ["--output", out]
  <> rest
  <> ["--json-errors"]

app :: Array String -> Array String -> Array String -> String -> EffN Unit
app files dirs ffi out = launchAff do
  let args = buildArgs ffi out files
  let globs = ffi <> files
  compile args
  watchAff dirs \path -> do
    when (any (minimatch path) globs) (compile args)

quoteJoin :: Array String -> String
quoteJoin xs = joinWith ", " (map (\v -> "'" <> v <> "'") xs)

main :: EffN Unit
main = do
  let setup = usage "psc-pane - Auto reloading PureScript compiler\n\nUsage: psc-pane [OPTION]"
              <> help "help" "Show this help"
              <> example "psc-pane" "Assume a pulp style project layout"
              <> example "psc-pane -w purs -s 'purs/**/*.purs' -f 'purs/**/*.js'"
                         "Having .purs and .js sources under 'purs'"
  let defaultPurs = ["src/**/*.purs", "bower_components/purescript-*/src/**/*.purs"]
  let defaultFFI = ["src/**/*.js", "bower_components/purescript-*/src/**/*.js"]
  runY setup $
    app
    <$> yarg "s" ["src"]
        (Just (".purs source file or glob pattern (default values: "
              <> (quoteJoin defaultPurs) <> ")"))
      (Left defaultPurs)
      true
    <*> yarg "w" ["watch-path"]
      (Just  "Directory to watch for changes (default: \"src\")")
      (Left ["src"])
      true
    <*> yarg "f" ["ffi"]
      (Just ("The input .js file(s) providing foreign import implementations (default values: "
             <> (quoteJoin defaultFFI) <> ")"))
      (Left defaultFFI)
      true
    <*> yarg "o" ["output"]
      (Just "The output directory (default: \"output\")")
      (Left "output")
      true
