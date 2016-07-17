module Blessed where

import Prelude
import Control.Monad.Eff (Eff)

foreign import data BLESSED ∷ !

foreign import data Screen ∷ *

foreign import data Box ∷ *

type ScreenOptions =
  { smartCSR ∷ Boolean
  }

foreign import mkScreen ∷ ScreenOptions → Screen

-- Append a box to a screen
foreign import append ∷ ∀ eff. Screen → Box → Eff (blessed ∷ BLESSED | eff) Unit

foreign import render ∷ ∀ eff. Screen → Eff (blessed ∷ BLESSED | eff) Unit

type BoxOptions =
  { width ∷ String
  , height ∷ String
  , content ∷ String
  }

foreign import mkBox ∷ BoxOptions → Box

foreign import setContent ∷ ∀ eff. Box → String → Eff (blessed ∷ BLESSED | eff) Unit
