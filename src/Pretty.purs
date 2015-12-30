module Pretty where

import Prelude ((<>), show, id, (>>>), (<<<), not, (==), (/=), (<=))
import Data.Array (head, length, take, dropWhile, takeWhile, filter)
import Data.String (joinWith, contains, split)
import Data.String.Regex (regex, replace)
import Data.Maybe (Maybe(..), maybe)
import Data.List (List(..), toList)

import Parse (Position(Position), PscError(PscError), PscResult(PscResult))

type Height = Int

pretty :: PscResult -> String
pretty (PscResult { warnings: [], errors: [] }) = "All good"
pretty (PscResult { warnings: [], errors: errors }) = maybe "" (prettyError 7) (head errors)
pretty (PscResult { warnings: warnings, errors: [] }) = maybe "" (prettyWarning 7) (head warnings)
pretty (PscResult { warnings: _, errors: errors }) = maybe "" (prettyError 7) (head errors)

prettyError' :: String -> Height -> PscError -> String
prettyError' t h (PscError err@{ position }) =
  t <> " " <> (filenameOrModule err) <> (prettyPosition position)
  <> "\n" <> (prettyMessage h err.message)

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
prettyMessage height lines = joinWith "\n" (fit height (trimLines lines))
  where
  fit height lines = maybe (take height lines) id (fitted lines)

  fitted :: Array String -> Maybe (Array String)
  fitted lines = try (toList [id, withoutExtraLines, withoutExtraLines >>> withoutWikiLink, withoutEmptyLines]) lines

  trimLines :: Array String -> Array String
  trimLines lines = dropWhile (== "") lines

  withoutExtraLines :: Array String -> Array String
  withoutExtraLines lines = split "\n" (replace (regex "\n{2}" flags) "\n" (joinWith "\n" lines))
    where flags = { global: true
                  , ignoreCase: false
                  , multiline: true
                  , sticky: false
                  , unicode: false }

  withoutWikiLink :: Array String -> Array String
  withoutWikiLink = takeWhile wikiLink
    where wikiLink = not <<< contains "See https://"

  withoutEmptyLines :: Array String -> Array String
  withoutEmptyLines = filter (/= "")

  try :: List (Array String -> Array String) -> Array String -> Maybe (Array String)
  try Nil _ = Nothing
  try (Cons f rest) lines =
    let res = f lines
    in if (length res) <= height
      then Just res
      else try rest lines

