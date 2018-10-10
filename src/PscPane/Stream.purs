module Stream where


import Prelude

import Control.Alt (class Alt)
import Control.Coroutine (Producer, consumer, runProcess, transform, ($$), ($~))
import Control.Coroutine as CO
import Control.Coroutine.Aff.Utils (mergeProducers)
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)


newtype Stream a = Stream (Producer a Aff Unit)

instance functorStream :: Functor Stream where
  map f (Stream p) = Stream (p $~ forever (transform f))

-- instance applyStream :: Apply Stream where
--   apply (Stream fa) (Stream p) =

instance altStream :: Alt Stream where
  alt (Stream x) (Stream y) = Stream (x `mergeProducers` y)

emit :: forall a. a -> Stream a
emit a = Stream (CO.emit a)

subscribe :: forall a. Stream a -> (a -> Aff Unit) -> Aff Unit
subscribe (Stream p) f = runProcess (p $$ consumer \a -> f a *> pure Nothing)