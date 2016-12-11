module PscPane.Parser where

import Prelude
import Data.Array (head, last)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)

import PscIde.Command (RebuildError)
import PscPane.State (PscFailure(..))

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

readPscJson ∷ String → Either String (Maybe PscFailure)
readPscJson err = case last lines of
  Nothing → Left "Got no lines from psc"
  Just json → jsonOutput json

  where
  lines ∷ Array String
  lines = split (Pattern "\n") $ trim err

  jsonOutput ∷ String → Either String (Maybe PscFailure)
  jsonOutput line = do
    json ← jsonParser line
    firstFailure <$> decodeJson json

  firstFailure ∷ PscResult → Maybe PscFailure
  firstFailure (PscResult { warnings: [], errors: [] }) = Nothing
  firstFailure (PscResult { warnings: [], errors: errors }) = Error <$> head errors
  firstFailure (PscResult { warnings: warnings, errors: [] }) = Warning <$> head warnings
  firstFailure (PscResult { warnings: _, errors: errors }) = Error <$> head errors
