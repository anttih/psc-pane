module PscPane.Watcher where

import Prelude

import Control.Coroutine.Aff (emit, produce)
import Effect (Effect)
import Stream (Stream(..))

foreign import watch' ∷ Array String → (String → Effect Unit) → Effect Unit

watch ∷ Array String → Stream String
watch dirs = Stream $ produce \emitter → watch' dirs (emit emitter)
