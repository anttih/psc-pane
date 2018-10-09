module Control.Coroutine.Aff.Utils where

import Prelude

import Control.Coroutine (Producer, consumer, runProcess, ($$))
import Control.Coroutine.Aff (close, emit, produceAff)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, forkAff, parallel, sequential)
import Effect.Aff.AVar as AV
import Effect.Exception (error)

mergeProducers :: forall o a. Producer o Aff a -> Producer o Aff a -> Producer o Aff Unit
mergeProducers l r = do
  var <- lift AV.empty

  let c = consumer \i -> AV.put i var *> pure Nothing

  void $ lift $ forkAff do
    void $ sequential $ parallel (runProcess (l $$ c)) *> parallel (runProcess (r $$ c))
    AV.kill (error "Both ended") var

  produceAff \emitter ->
    let
      go = do
        status <- AV.status var
        if AV.isKilled status
          then close emitter unit
          else do
            a <- AV.take var
            emit emitter a
            go
    in go
