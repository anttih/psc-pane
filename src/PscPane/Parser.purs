module PscPane.Parser where

import Prelude
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import PscIde.Command (RebuildError)

newtype PscResult = PscResult
  { warnings ∷ Array RebuildError
  , errors ∷ Array RebuildError
  }

instance isDecodeJsonPscResult ∷ DecodeJson PscResult where
  decodeJson json = do
    j ← decodeJson json
    warnings ← j .? "warnings"
    errors ← j .? "errors"
    pure $ PscResult { warnings, errors }
