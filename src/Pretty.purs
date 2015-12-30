module Pretty where

import Prelude ((<>), show, id, (<<<), not, (==), (/=), (<=), (-))
import Data.Array (head, length, take, dropWhile, takeWhile, filter)
import Data.String (joinWith, contains, split, trim)
import Data.String.Regex (regex, replace)
import Data.Maybe (Maybe(..), maybe)
import Data.List (List(..), toList)

import Parse (Position(Position), PscError(PscError), PscResult(PscResult))

type Height = Int

pretty :: Height -> PscResult -> String
pretty h (PscResult { warnings: [], errors: [] }) = "All good"
pretty h (PscResult { warnings: [], errors: errors }) = maybe "" (prettyError h) (head errors)
pretty h (PscResult { warnings: warnings, errors: [] }) = maybe "" (prettyWarning h) (head warnings)
pretty h (PscResult { warnings: _, errors: errors }) = maybe "" (prettyError h) (head errors)

prettyError' :: String -> Height -> PscError -> String
prettyError' t h (PscError err@{ position }) =
  t <> " " <> (filenameOrModule err) <> (prettyPosition position)
  <> "\n" <> (prettyMessage (h - 1) err.message)

prettyError :: Height -> PscError -> String
prettyError = prettyError' "Error"

prettyWarning :: Height -> PscError -> String
prettyWarning = prettyError' "Warning"

filenameOrModule :: forall xs. { filename :: Maybe String, moduleName :: Maybe String | xs } -> String
filenameOrModule { filename: Just file } = file
filenameOrModule { moduleName: Just moduleName } = moduleName
filenameOrModule _ = ""

prettyPosition :: Maybe Position -> String
prettyPosition (Just (Position { startLine, startColumn })) =
  ":" <> (show startLine) <> ":" <> (show startColumn)
prettyPosition Nothing = ""

prettyMessage :: Height -> Array String -> String
prettyMessage height lines = joinWith "\n" (fit height lines)
  where
  -- | Either we can fit the message intelligently or we just give up and
  -- | force the height
  fit :: Height -> Array String -> Array String
  fit height lines = maybe (take height lines) id (fitted lines)

  -- | Our fitting strategy
  fitted :: Array String -> Maybe (Array String)
  fitted lines = try (toList [ id
                             , trimLines
                             , withoutExtraLines
                             , withoutWikiLink
                             , withoutEmptyLines
                             , withoutTypeInfo
                             ]) lines

  trimLines :: Array String -> Array String
  trimLines lines = split "\n" (replace (regex "^\n+|\n+$" flags) "" (joinWith "\n" lines))
    where flags = { global: true
                  , ignoreCase: false
                  , multiline: false
                  , sticky: false
                  , unicode: false }


  withoutExtraLines :: Array String -> Array String
  withoutExtraLines lines = split "\n" (replace (regex "\n{2}" flags) "\n" (joinWith "\n" lines))
    where flags = { global: true
                  , ignoreCase: false
                  , multiline: true
                  , sticky: false
                  , unicode: false }

  withoutWikiLink :: Array String -> Array String
  withoutWikiLink = trimLines <<< takeWhile wikiLink
    where wikiLink = not <<< contains "See https://"

  withoutEmptyLines :: Array String -> Array String
  withoutEmptyLines = filter ((/= "") <<< trim)

  withoutTypeInfo :: Array String -> Array String
  withoutTypeInfo  = trimLines <<< takeWhile typeInfo
    where typeInfo = not <<< contains "while trying to match type"

  -- | Try different fitting functions passing the previous result to the next
  -- | until the message fits.
  try :: List (Array String -> Array String) -> Array String -> Maybe (Array String)
  try Nil _ = Nothing
  try (Cons f rest) lines =
    let res = f lines
    in if (length res) <= height
      then Just res
      else try rest res

