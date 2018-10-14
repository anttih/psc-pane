module PscPane.Interpreter where

import Prelude

import Blessed (render, setContent)
import Control.Monad.Error.Class (throwError)
import Data.Array (head)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Class.Console as Console
import Effect.Exception (error, throwException)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Node.Process (exit) as P
import PscIde (load, rebuild) as Ide
import PscIde.Command (RebuildResult(..))
import PscIde.Server (stopServer) as Ide
import PscPane.Config (Config)
import PscPane.Output (display)
import PscPane.Pretty (formatState)
import PscPane.Program (ACTION, ActionF(..), _action)
import PscPane.Program (ExitReason(..)) as P
import PscPane.Spawn (spawn)
import PscPane.State (PscFailure(..))
import Run (AFF, EFFECT, Run, interpret, liftAff, liftEffect, on, runBaseAff', send)
import Run.Except (EXCEPT, catch)

appN ∷ Ref Config -> ActionF ~> Run (aff :: AFF, effect :: EFFECT)
appN ref = case _ of

  RebuildModule path f -> do
    { port } <- liftEffect $ Ref.read ref
    res ← liftAff $ Ide.rebuild port path Nothing
    either (liftAff <<< throwError <<< error) (pure <<< f <<< takeOne) res

  LoadModules a -> do
    { port } ← liftEffect $ Ref.read ref
    const a <$> liftAff (Ide.load port [] [])

  Spawn command args f -> do
    res <- liftAff $ spawn command args
    pure (f res)

  DrawPaneState state a -> do
    { screen, box, cwd, options: { colorize } } ← liftEffect $ Ref.read ref
    height ← liftEffect rows
    liftEffect (setContent box (formatState colorize cwd height state))
    liftEffect (render screen)
    void $ liftEffect $ Ref.modify (_ { prevPaneState = state }) ref
    pure a

  ShowError err a ->
    const a <$> liftAff (display err)

  Ask f -> do
    config <- liftEffect $ Ref.read ref
    pure (f config)

  where

  takeOne ∷ Either RebuildResult RebuildResult → Maybe PscFailure
  takeOne (Right (RebuildResult warnings)) = Warning <$> head warnings
  takeOne (Left (RebuildResult errors)) = Error <$> head errors

catch' :: forall r a. Ref Config -> P.ExitReason -> Run (effect :: EFFECT, aff :: AFF | r) a
catch' ref reason = do
  { port } <- liftEffect $ Ref.read ref
  void $ liftAff $ attempt $ Ide.stopServer port
  case reason of
    P.Exit -> do
      void $ liftEffect $ P.exit 0
      liftEffect $ throwException (error "boom")
    P.Error msg -> do
      -- destroy screen
      liftAff $ Console.error $ "Error: " <> msg
      void $ liftEffect $ P.exit (-1)
      liftEffect $ throwException (error "boom")

run ∷ ∀ a. Ref Config → Run (action :: ACTION, except :: EXCEPT P.ExitReason, effect :: EFFECT, aff :: AFF) a → Aff a
run ref p =
  catch (catch' ref) p
    # interpret (on _action (appN ref) send)
    # runBaseAff'

foreign import rows ∷ Effect Int
