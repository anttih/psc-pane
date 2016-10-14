module PscPane.Watcher where

import Prelude
import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce)
import Data.Either (Either(Left))
import PscPane.Types (AffN, EffN)

foreign import watch' ∷ Array String → (String → EffN Unit) → EffN Unit

watch ∷ Array String → Producer String AffN Unit
watch dirs = produce \emit → watch' dirs (emit <<< Left)
