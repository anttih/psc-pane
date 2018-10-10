module PscPane.Interpreter where

import Prelude

import Blessed (render, setContent)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Free (foldFree)
import Control.Monad.State.Class (get, modify)
import Control.Monad.State.Trans (StateT, lift, execStateT)
import Data.Array (head)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error, throwException)
import Node.Process as P
import PscIde (load, rebuild) as Ide
import PscIde.Command (RebuildResult(..))
import PscIde.Server (stopServer) as Ide
import PscPane.Config (Config)
import PscPane.DSL (DSL, ActionF(..))
import PscPane.DSL as Dsl
import PscPane.Output (display)
import PscPane.Pretty (formatState)
import PscPane.Spawn (spawn)
import PscPane.State (PscFailure(..))

appN ∷ ActionF ~> StateT Config Aff
appN = case _ of

  RebuildModule path f -> do
    { port } ← get
    res ← lift $ Ide.rebuild port path Nothing
    either (throwError <<< error) (pure <<< f <<< takeOne) res

  LoadModules a -> do
    { port } ← get
    lift $ const a <$> Ide.load port [] []

  Spawn command args f -> do
    res <- lift (spawn command args)
    pure (f res)

  DrawPaneState state a -> do
    { screen, box, cwd, options: { colorize } } ← get
    height ← liftEffect rows
    liftEffect (setContent box (formatState colorize cwd height state))
    liftEffect (render screen)
    void $ modify (_ { prevPaneState = state })
    pure a

  ShowError err a ->
    lift $ const a <$> display err

  Exit reason -> do
    { port } <- get
    void $ lift $ attempt $ Ide.stopServer port
    case reason of
      Dsl.Quit -> do
        void $ liftEffect $ P.exit 0
        liftEffect $ throwException (error "boom")
      Dsl.Error msg -> do
        -- destroy screen
        Console.error $ "Error: " <> msg
        void $ liftEffect $ P.exit (-1)
        liftEffect $ throwException (error "boom")

  Ask f -> do
    config <- get
    pure (f config)

  where

  takeOne ∷ Either RebuildResult RebuildResult → Maybe PscFailure
  takeOne (Right (RebuildResult warnings)) = Warning <$> head warnings
  takeOne (Left (RebuildResult errors)) = Error <$> head errors


run ∷ ∀ a. Config → DSL a → Aff Config
run state program = execStateT (foldFree appN program) state

foreign import rows ∷ Effect Int
