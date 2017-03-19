module Blessed where

import Prelude
import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Data.Either (Either(Left))

foreign import data BLESSED ∷ !

foreign import data Screen ∷ *

foreign import data Box ∷ *

type ScreenOptions =
  { smartCSR ∷ Boolean
  , debug ∷ Boolean
  }

foreign import mkScreen ∷ ScreenOptions → Screen

-- Append a box to a screen
foreign import append ∷ ∀ eff. Screen → Box → Eff (blessed ∷ BLESSED | eff) Unit

foreign import render ∷ ∀ eff. Screen → Eff (blessed ∷ BLESSED | eff) Unit

foreign import debug ∷ ∀ eff. Screen → String → Eff (blessed ∷ BLESSED | eff) Unit

onResize ∷ ∀ eff. Screen → Producer Unit (Aff (avar ∷ AVAR, blessed ∷ BLESSED | eff)) Unit
onResize screen = produce \emit → on screen "resize" (emit <<< Left)

onQuit
  ∷ ∀ eff. Screen
  → Array String
  → Producer Unit (Aff (avar ∷ AVAR, blessed ∷ BLESSED | eff)) Unit
onQuit screen keys = produce \emit → key screen keys (emit <<< Left)

foreign import on
  ∷ ∀ eff. Screen
  → String
  → (Unit → Eff (blessed ∷ BLESSED | eff) Unit)
  → Eff (blessed ∷ BLESSED | eff) Unit

foreign import key
  ∷ ∀ eff. Screen
  → Array String
  → (Unit → Eff (blessed ∷ BLESSED | eff) Unit)
  → Eff (blessed ∷ BLESSED | eff) Unit

type BoxOptions =
  { width ∷ String
  , height ∷ String
  , content ∷ String
  }

foreign import mkBox ∷ BoxOptions → Box

foreign import setContent ∷ ∀ eff. Box → String → Eff (blessed ∷ BLESSED | eff) Unit

foreign import destroy ∷ ∀ eff. Screen → Eff (blessed ∷ BLESSED | eff) Unit
