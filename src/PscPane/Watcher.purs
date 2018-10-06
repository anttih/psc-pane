module PscPane.Watcher where

import Prelude

import Control.Coroutine (Producer)
import Control.Coroutine.Aff (emit, produce)
import PscPane.Types (AffN, EffN)

foreign import watch' ∷ Array String → (String → EffN Unit) → EffN Unit

watch ∷ Array String → Producer String AffN Unit
watch dirs = produce \emitter → watch' dirs (emit emitter)
