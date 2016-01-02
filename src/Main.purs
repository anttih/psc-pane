module Main where

import Prelude (Unit, bind, (<*>), (<$>), ($), (<>), unit, pure, show)
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

type ChildProcess =
  { stdout :: Buffer
  , stderr :: Buffer
}

foreign import exec
  :: String
  -> (Error -> EffN Unit)
  -> (ChildProcess -> EffN Unit)
  -> EffN Unit

execAff :: String -> AffN ChildProcess
execAff cmd = makeAff (\error success -> exec cmd error success)

foreign import write :: String -> EffN Unit

clear :: AffN Unit
clear = do
  liftEff (write "\x1b[2J")
  liftEff (write "\x1b[1;1H")
  pure unit

compile :: String -> AffN Unit
compile cmd = do
  clear
  log "Compiling..."
  { stderr, stdout } <- execAff cmd
  
  height <- liftEff rows

  clear

  dir <- liftEff cwd

  err <- liftEff (toString UTF8 stderr)
  liftEff $ write (either show (pretty dir height) (readJSON err))

  pure unit

foreign import rows :: EffN Int

foreign import shellEscape :: Array String -> String

buildCmd :: Array String -> String -> Array String -> String
buildCmd ffi out rest = shellEscape
  $ ["psc"]
  <> concatMap (\f -> ["--ffi", f]) ffi
  <> ["--output", out]
  <> rest
  <> ["--json-errors"]

app :: Array String -> String -> Array String -> String -> EffN Unit
app files src ffi out = launchAff do
  let cmd' = buildCmd ffi out files
  compile cmd'
  watchAff [src] \path -> do
    when (any (minimatch path) files) (compile cmd')

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
      (Left ["src/**/*.js", "bower_components/pursescript-*/src/**/*.js"]) true
    <*> yarg "o" ["output"] (Just "The output directory (default: \"output\")")
      (Left "output") true
