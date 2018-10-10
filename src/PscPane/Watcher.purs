module PscPane.Watcher where

import Prelude

import Control.Coroutine.Aff (emit, produce)
import Effect (Effect)
import Stream (Stream(..))

foreign import watch' ∷ Array String → (String → Effect Unit) → Effect Unit

onFileChange ∷ Array String → Stream String
onFileChange dirs = Stream $ produce \emitter → watch' dirs (emit emitter)
