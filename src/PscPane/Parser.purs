module PscPane.Parser where

import Prelude
import Data.Array (head)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Parser (jsonParser)
import Data.Foldable (fold)
import Data.Maybe.First (First(..))
import Data.Either (Either(..))
import Data.Newtype (unwrap)
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

readPscJson ∷ String → Maybe (Maybe PscFailure)
readPscJson err = findFirst jsonOutput lines
  where
  lines ∷ Array String
  lines = split (Pattern "\n") $ trim err

  jsonOutput ∷ String → Maybe (Maybe PscFailure)
  jsonOutput line = eitherToMaybe do
    json ← jsonParser line
    firstFailure <$> decodeJson json

  eitherToMaybe ∷ forall e a. Either e a → Maybe a
  eitherToMaybe (Right a) = Just a
  eitherToMaybe _ = Nothing

  firstFailure ∷ PscResult → Maybe PscFailure
  firstFailure (PscResult { warnings: [], errors: [] }) = Nothing
  firstFailure (PscResult { warnings: [], errors: errors }) = Error <$> head errors
  firstFailure (PscResult { warnings: warnings, errors: [] }) = Warning <$> head warnings
  firstFailure (PscResult { warnings: _, errors: errors }) = Error <$> head errors

findFirst ∷ ∀ a. (String → Maybe a) → Array String → Maybe a
findFirst f xs = unwrap (fold (map (First <<< f) xs))

