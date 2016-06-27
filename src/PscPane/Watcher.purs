module PscPane.Watcher
  (watch) where

import Prelude
import PscPane.Types (EffN)

foreign import watch :: Array String -> (String -> EffN Unit) -> EffN Unit
