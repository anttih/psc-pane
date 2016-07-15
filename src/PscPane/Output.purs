module PscPane.Output where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import PscPane.Types (AffN, EffN)

clear :: AffN Unit
clear = do
  liftEff $ write "\x1b[2J"
  liftEff $ write "\x1b[1;1H"
  pure unit

display ∷ String → AffN Unit
display content = clear *> liftEff (write content)

foreign import write :: String -> EffN Unit
