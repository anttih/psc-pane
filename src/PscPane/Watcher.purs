module PscPane.Watcher
  (watchAff) where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (launchAff)
import PscPane.Types (EffN, AffN)

watchAff :: Array String -> (String -> AffN Unit) -> AffN Unit
watchAff dirs callback =
  liftEff (watch dirs (\path -> launchAff (callback path)))

foreign import watch :: Array String -> (String -> EffN Unit) -> EffN Unit
