module PscPane.Pretty where

import Prelude
import Data.Either (Either(Right))
import Data.Array (length, take, takeWhile, filter)
import Data.String (joinWith, contains, split, trim)
import Data.String.Regex as R
import Data.Maybe (Maybe(..), maybe)
import Data.List (List(..), fromFoldable)
import Node.Path (FilePath, relative)

import PscIde.Command (RebuildError(..))
import PscPane.Color (green, yellow, red)

type Height = Int

type Progress = String

data PaneState
  = InitialBuild
  | BuildSuccess
  | ModuleOk FilePath Progress
  | PscError PaneResult

formatState ∷ FilePath → Height → PaneState → String
formatState _ _ InitialBuild = "Building project..."
formatState cwd height (PscError res) = pretty cwd height res
formatState _ _ (ModuleOk path progress) = green "Module OK" <> " " <> path <> " (" <> progress <> ")"
formatState _ _ BuildSuccess = green "Build successful"

data PaneResult = Warning RebuildError | Error RebuildError

pretty ∷ FilePath → Height → PaneResult → String
pretty cwd h (Warning warn) = prettyError' (yellow "Warning") cwd h warn
pretty cwd h (Error err) = prettyError' (red "Error") cwd h err

prettyError' ∷ String → FilePath → Height → RebuildError → String
prettyError' t cwd h (RebuildError err@{ position }) =
  t <> " " <> (filenameOrModule cwd err) <> (prettyPosition position)
  <> "\n" <> (prettyMessage (h - 1) (split "\n" err.message))

filenameOrModule ∷ forall xs
  . FilePath
  → { filename ∷ Maybe String , moduleName ∷ Maybe String | xs }
  → String
filenameOrModule cwd { filename: Just file } = relative cwd file
filenameOrModule _ { moduleName: Just moduleName } = moduleName
filenameOrModule _ _ = ""

prettyPosition ∷ Maybe { line ∷ Int, column ∷ Int } → String
prettyPosition (Just { line, column }) =
  " line " <> (show line) <> ", column " <> (show column)
prettyPosition Nothing = ""

prettyMessage ∷ Height → Array String → String
prettyMessage height lines = joinWith "\n" (fit height lines)
  where
  -- | Either we can fit the message intelligently or we just give up and
  -- | force the height
  fit ∷ Height → Array String → Array String
  fit height lines = maybe (take height lines) id (fitted lines)

  -- | Our fitting strategy
  fitted ∷ Array String → Maybe (Array String)
  fitted lines = try (fromFoldable [ id
                                   , trimLines
                                   , withoutExtraLines
                                   , withoutWikiLink
                                   , withoutEmptyLines
                                   , withoutTypeInfo
                                   , take height
                                   ]) lines

  trimLines ∷ Array String → Array String
  trimLines lines = replace (R.regex "^\n+|\n+$" flags) "" lines
    where flags = { global: true
                  , ignoreCase: false
                  , multiline: false
                  , sticky: false
                  , unicode: false }


  withoutExtraLines ∷ Array String → Array String
  withoutExtraLines lines = replace (R.regex "\n{2}" flags) "\n" lines
    where flags = { global: true
                  , ignoreCase: false
                  , multiline: true
                  , sticky: false
                  , unicode: false }

  replace ∷ Either String R.Regex → String → Array String → Array String
  replace (Right regex) s lines = split "\n" (R.replace regex s (joinWith "\n" lines))
  replace _ _ lines = lines

  withoutWikiLink ∷ Array String → Array String
  withoutWikiLink = trimLines <<< takeWhile wikiLink
    where wikiLink = not <<< contains "See https://"

  withoutEmptyLines ∷ Array String → Array String
  withoutEmptyLines = filter ((_ /= "") <<< trim)

  withoutTypeInfo ∷ Array String → Array String
  withoutTypeInfo  = trimLines <<< takeWhile typeInfo
    where typeInfo = not <<< contains "while trying to match type"

  -- | Try different fitting functions passing the previous result to the next
  -- | until the message fits.
  try ∷ List (Array String → Array String) → Array String → Maybe (Array String)
  try Nil _ = Nothing
  try (Cons f rest) lines =
    let res = f lines
    in if (length res) <= height
      then Just res
      else try rest res
