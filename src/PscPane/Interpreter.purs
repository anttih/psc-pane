module PscPane.Interpreter where

import Prelude
import Control.Monad.Aff (makeAff)
import Control.Monad.Free (foldFree)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.State.Trans (StateT, lift, execStateT)
import Control.Monad.State.Class (get, modify)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Either (Either(..), either)
import Data.String (Pattern(..), Replacement(..), replace)
import Node.ChildProcess (Exit(BySignal, Normally), defaultSpawnOptions,
                          onClose, stdout, stderr)
import Node.ChildProcess as CP
import Node.Stream (onDataString)
import Node.Encoding (Encoding(UTF8))
import Node.Path as Path
import PscIde (load, rebuild)
import PscIde.Command (RebuildResult(..))

import Blessed (render, setContent)
import PscPane.Config (Config)
import PscPane.Types (EffN, AffN)
import PscPane.Parser (readPscJson)
import PscPane.Pretty (formatState)
import PscPane.Output (display)
import PscPane.DSL (ActionF(..), Action)
import PscPane.State (PscFailure(..))


appN ∷ ActionF ~> StateT Config AffN
appN (RebuildModule path f) = do
  { port } ← get
  res ← lift $ rebuild port path
  either (throwError <<< error) (pure <<< f <<< takeOne) res

  where
  takeOne ∷ Either RebuildResult RebuildResult → Maybe PscFailure
  takeOne (Right (RebuildResult warnings)) = Warning <$> head warnings
  takeOne (Left (RebuildResult errors)) = Error <$> head errors

appN (LoadModules a) = do
  { port } ← get
  lift $ const a <$> load port [] []
appN (BuildProject f) = do
  { screen, box, srcPath, libPath, testPath, test } ← get
  let srcGlob = Path.concat [srcPath, "**", "*.purs"]
      libGlob = Path.concat [libPath, "purescript-*", "src", "**", "*.purs"]
      testSrcGlob = Path.concat [testPath, "**", "*.purs"]
      args
        = ["--json-errors", srcGlob, libGlob]
        <> if test then pure testSrcGlob else mempty

  child ← liftEff $ CP.spawn "psc" args defaultSpawnOptions
  output ← liftEff $ newRef ""
  res ← lift $ makeAff \_ success -> do
    onDataString (stderr child) UTF8 \s → do
      current ← readRef output
      writeRef output (current <> s)
    onClose child \c → readRef output >>= success

  case readPscJson res of
    Nothing →
      let msg = "Could not read psc output. Make sure you use the --json-errors flag for psc."
      in throwError (error msg)
    Just res' → pure (f res')
appN (RunTests f) = do
  { testMain } ← get
  let modulePath = "./output/" <> jsEscape testMain
  child ← liftEff $
    CP.spawn "node" ["-e", "require('" <> modulePath <> "').main();"] defaultSpawnOptions
  errRef ← liftEff $ newRef ""
  stdRef ← liftEff $ newRef ""
  res ← lift $ makeAff \_ succ -> do
    onDataString (stdout child) UTF8 \s → do
      current ← readRef stdRef
      writeRef stdRef (current <> s)

    onDataString (stderr child) UTF8 \s → do
      current ← readRef errRef
      writeRef errRef (current <> s)

    onClose child \c → case c of
      (Normally 0) → do
        stdOut ← readRef stdRef
        stdErr ← readRef errRef
        succ $ Right { stdOut, stdErr }
      (Normally n) → do
        stdOut ← readRef stdRef
        stdErr ← readRef errRef
        succ $ Left { stdOut, stdErr }
      (BySignal _) → succ $ Left { stdErr: "Signal interrupted test", stdOut: "" }
  pure $ f res

  where
  -- | This is from bodil/pulp
  -- | 
  -- | Escape a string for insertion into a JS string literal.
  jsEscape :: String -> String
  jsEscape =
    replace (Pattern "'") (Replacement "\\'")
    <<< replace (Pattern "\\") (Replacement "'")

appN (ShouldRunTests f) = do
  { test } ← get
  pure (f test)
appN (DrawPaneState state a) = do
  { screen, box, cwd, colorize } ← get
  height ← liftEff rows
  liftEff (setContent box (formatState colorize cwd height state))
  liftEff (render screen)
  modify (_ { prevPaneState = state })
  pure a
appN (ShowError err a) = lift $ const a <$> display err

run ∷ ∀ a. Config → Action a → AffN Config
run state program = execStateT (foldFree appN program) state

foreign import rows ∷ EffN Int
