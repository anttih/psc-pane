module PscPane.Output where

import Prelude
import Effect.Class (liftEffect)
import PscPane.Types (AffN, EffN)

clear ∷ EffN Unit
clear = do
  write "\x1b[2J"
  write "\x1b[1;1H"
  pure unit

display ∷ String → AffN Unit
display content = liftEffect clear *> liftEffect (write content)

foreign import write ∷ String → EffN Unit
