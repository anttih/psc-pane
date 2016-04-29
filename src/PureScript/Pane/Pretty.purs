module PureScript.Pane.Pretty where

import Prelude
import Data.Array (head, length, null, take, takeWhile, filter)
import Data.String (joinWith, contains, split, trim)
import Data.String.Regex as R
import Data.Maybe (Maybe(..), maybe)
import Data.List (List(..), toList)
import Ansi.Codes ( EscapeCode(Graphics)
                  , GraphicsParam(PBackground, PForeground, Reset)
                  , Color(Black, Green, Yellow, Red, White)
                  , escapeCodeToString)
import Node.Path (FilePath, relative)

import PscIde.Command (RebuildError(..))
import PureScript.Pane.Parser (PscResult(PscResult))

type Height = Int

pretty :: FilePath -> Height -> PscResult -> String
pretty cwd h (PscResult { warnings: [], errors: [] }) = green "All good"
pretty cwd h (PscResult { warnings: warnings, errors: errors }) = maybe "" id $
  if not (null errors)
    then prettyError cwd h <$> head errors
    else prettyWarning cwd h <$> head warnings

prettyError' :: String -> FilePath -> Height -> RebuildError -> String
prettyError' t cwd h (RebuildError err@{ position }) =
  t <> " " <> (filenameOrModule cwd err) <> (prettyPosition position)
  <> "\n" <> (prettyMessage (h - 1) (split "\n" err.message))

prettyError :: FilePath -> Height -> RebuildError -> String
prettyError = prettyError' (red "Error")

prettyWarning :: FilePath -> Height -> RebuildError -> String
prettyWarning = prettyError' (yellow "Warning")

filenameOrModule :: forall xs
  . FilePath
  -> { filename :: Maybe String , moduleName :: Maybe String | xs }
  -> String
filenameOrModule cwd { filename: Just file } = relative cwd file
filenameOrModule _ { moduleName: Just moduleName } = moduleName
filenameOrModule _ _ = ""

prettyPosition :: Maybe { line :: Int, column :: Int } -> String
prettyPosition (Just { line, column }) =
  " line " <> (show line) <> ", column " <> (show column)
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
                             , take height
                             ]) lines

  trimLines :: Array String -> Array String
  trimLines lines = replace (R.regex "^\n+|\n+$" flags) "" lines
    where flags = { global: true
                  , ignoreCase: false
                  , multiline: false
                  , sticky: false
                  , unicode: false }


  withoutExtraLines :: Array String -> Array String
  withoutExtraLines lines = replace (R.regex "\n{2}" flags) "\n" lines
    where flags = { global: true
                  , ignoreCase: false
                  , multiline: true
                  , sticky: false
                  , unicode: false }

  replace :: R.Regex -> String -> Array String -> Array String
  replace regex s lines = split "\n" (R.replace regex s (joinWith "\n" lines))

  withoutWikiLink :: Array String -> Array String
  withoutWikiLink = trimLines <<< takeWhile wikiLink
    where wikiLink = not <<< contains "See https://"

  withoutEmptyLines :: Array String -> Array String
  withoutEmptyLines = filter ((_ /= "") <<< trim)

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

--
-- Color stuff
--

withColor :: Array GraphicsParam -> String -> String
withColor params s =
  escapeCodeToString (Graphics params) <> s <> escapeCodeToString (Graphics [Reset])

green :: String -> String
green = withColor [PBackground Green, PForeground White]

yellow :: String -> String
yellow = withColor [PBackground Yellow, PForeground Black]

red :: String -> String
red = withColor [PBackground Red, PForeground White]

