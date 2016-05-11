module PscPane.Parser where

import Data.Argonaut.Combinators ((.?))
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Prelude (($), bind, pure)
import PscIde.Command (RebuildError)

newtype PscResult = PscResult
  { warnings :: Array RebuildError
  , errors :: Array RebuildError
  }

instance isForeignPscResult :: DecodeJson PscResult where
  decodeJson json = do
    j <- decodeJson json
    warnings <- j .? "warnings"
    errors <- j .? "errors"
    pure $ PscResult { warnings, errors }
