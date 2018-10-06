module PscPane.Output where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

clear ∷ Effect Unit
clear = do
  write "\x1b[2J"
  write "\x1b[1;1H"
  pure unit

display ∷ String → Aff Unit
display content = liftEffect clear *> liftEffect (write content)

foreign import write ∷ String → Effect Unit
