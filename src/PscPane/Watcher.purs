module PscPane.Watcher where

import Prelude

import Control.Coroutine (Producer)
import Control.Coroutine.Aff (emit, produce)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import watch' ∷ Array String → (String → Effect Unit) → Effect Unit

watch ∷ Array String → Producer String Aff Unit
watch dirs = produce \emitter → watch' dirs (emit emitter)
