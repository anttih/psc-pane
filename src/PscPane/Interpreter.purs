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
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Newtype (unwrap)
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Data.Foldable (fold)
import Data.Either (Either(..), either)
import Data.String (Pattern(..), Replacement(..), split, trim, replace)
import Node.ChildProcess (Exit(BySignal, Normally), defaultSpawnOptions, onClose, stderr)
import Node.ChildProcess as CP
import Node.Stream (onDataString)
import Node.Encoding (Encoding(UTF8))
import Node.Path as Path
import PscIde (load, rebuild)
import PscIde.Command (RebuildResult(..))

import Blessed (render, setContent)
import PscPane.Config (Config)
import PscPane.Types (EffN, AffN)
import PscPane.Parser (PscResult(..))
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
  { screen, box, srcPath, libPath } ← get
  let srcGlob = Path.concat [srcPath, "**", "*.purs"]
      libGlob = Path.concat [libPath, "purescript-*", "src", "**", "*.purs"]
  child ← liftEff $ CP.spawn "psc" [srcGlob, libGlob, "--json-errors"] defaultSpawnOptions
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
  output ← liftEff $ newRef ""
  res ← lift $ makeAff \_ succ -> do
    onDataString (stderr child) UTF8 \s → do
      current ← readRef output
      writeRef output (current <> s)

    onClose child \c → case c of
      (Normally 0) → readRef output >>= const (succ Nothing)
      (Normally n) → readRef output >>= succ <<< Just
      (BySignal _) → succ $ Just "Signal interrupted test"
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

readPscJson ∷ String → Maybe (Maybe PscFailure)
readPscJson err = findFirst jsonOutput lines
  where
  lines ∷ Array String
  lines = split (Pattern "\n") $ trim err

  jsonOutput ∷ String → Maybe (Maybe PscFailure)
  jsonOutput line = eitherToMaybe do
    json ← jsonParser line
    firstFailure <$> decodeJson json

  eitherToMaybe ∷ forall e a. Either e a → Maybe a
  eitherToMaybe (Right a) = Just a
  eitherToMaybe _ = Nothing

  firstFailure ∷ PscResult → Maybe PscFailure
  firstFailure (PscResult { warnings: [], errors: [] }) = Nothing
  firstFailure (PscResult { warnings: [], errors: errors }) = Error <$> head errors
  firstFailure (PscResult { warnings: warnings, errors: [] }) = Warning <$> head warnings
  firstFailure (PscResult { warnings: _, errors: errors }) = Error <$> head errors

findFirst ∷ ∀ a. (String → Maybe a) → Array String → Maybe a
findFirst f xs = unwrap (fold (map (First <<< f) xs))

run ∷ ∀ a. Config → Action a → AffN Config
run state program = execStateT (foldFree appN program) state

foreign import rows ∷ EffN Int
