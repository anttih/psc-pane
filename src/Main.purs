module Main where

import Prelude (Unit, bind, (<*>), (<$>), ($), (<>), unit, pure, const)
import Control.Monad (when)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (Error(), EXCEPTION())
import Control.Monad.Aff (Aff(), makeAff, launchAff)
import Control.Monad.Aff.Console (log)
import Data.Array (concatMap)
import Data.Foldable (any)
import Data.Foreign.Class (readJSON)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Function (Fn4(), runFn4)

import Node.FS (FS())
import Node.Buffer (Buffer(), BUFFER(), toString)
import Node.Encoding (Encoding(UTF8))
import Node.Process (PROCESS, cwd)

import Node.Yargs.Setup (usage, demandCount)
import Node.Yargs.Applicative (Y(), arg, yarg, runY)

import Pretty (pretty)

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

compile :: Array String -> AffN Unit
compile args = do
  clear
  log "Compiling..."
  buf <- spawnAff "psc" args
  height <- liftEff rows
  clear
  dir <- liftEff cwd
  err <- liftEff (toString UTF8 buf)
  let errorMsg = "Error reading psc output."
  liftEff $ write (either (const err) (pretty dir height) (readJSON err))
  pure unit

foreign import rows :: EffN Int

buildArgs :: Array String -> String -> Array String -> Array String
buildArgs ffi out rest =
  concatMap (\f -> ["--ffi", f]) ffi
  <> ["--output", out]
  <> rest
  <> ["--json-errors"]

app :: Array String -> String -> Array String -> String -> EffN Unit
app files src ffi out = launchAff do
  let args = buildArgs ffi out files
  compile args
  watchAff [src] \path -> do
    when (any (minimatch path) files) (compile args)

-- | Read all non-hyphenated args as strings
nonHyphen :: Y (Array String)
nonHyphen = arg "_"

main :: EffN Unit
main = do
  let setup = usage "psc-pane [FILE] [-f|--ffi ARG] [-o|--output ARG]"
        <> demandCount 1 "No input files."
  runY setup $
    app
    <$> nonHyphen
    <*> yarg "s" ["src-path"] (Just "path") (Left "src") false
    <*> yarg "f" ["ffi"] (Just "The input .js file(s) providing foreign import implementations")
      (Left []) false
    <*> yarg "o" ["output"] (Just "The output directory (default: \"output\")")
      (Left "output") true
