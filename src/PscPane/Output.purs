module PscPane.Output where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import PscPane.Types (AffN, EffN)

clear ∷ EffN Unit
clear = do
  write "\x1b[2J"
  write "\x1b[1;1H"
  pure unit

display ∷ String → AffN Unit
display content = liftEff clear *> liftEff (write content)

foreign import write ∷ String → EffN Unit
