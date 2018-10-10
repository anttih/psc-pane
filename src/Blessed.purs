module Blessed where

import Prelude

import Control.Coroutine.Aff (emit, produce)
import Effect (Effect)
import Stream (Stream(..))

foreign import data Screen ∷ Type

foreign import data Box ∷ Type

type ScreenOptions =
  { smartCSR ∷ Boolean
  , debug ∷ Boolean
  }

foreign import mkScreen ∷ ScreenOptions → Screen

-- Append a box to a screen
foreign import append ∷ Screen → Box → Effect Unit

foreign import render ∷ Screen → Effect Unit

foreign import debug ∷ Screen → String → Effect Unit

onResize ∷ Screen → Stream Unit
onResize screen = Stream $ produce \emitter → on screen "resize" (emit emitter)

onQuit ∷ Screen → Array String → Stream Unit
onQuit screen keys = Stream $ produce \emitter → key screen keys (emit emitter)

foreign import on
  ∷ Screen
  → String
  → (Unit → Effect Unit)
  → Effect Unit

foreign import key
  ∷ Screen
  → Array String
  → (Unit → Effect Unit)
  → Effect Unit

type BoxOptions =
  { width ∷ String
  , height ∷ String
  , content ∷ String
  , scrollable ∷ Boolean
  , scrollbar ∷ Boolean
  , alwaysScroll ∷ Boolean
  , keys ∷ Boolean
  , vi ∷ Boolean
  }

foreign import mkBox ∷ BoxOptions → Box

foreign import setContent ∷ Box → String → Effect Unit

foreign import destroy ∷ Screen → Effect Unit
