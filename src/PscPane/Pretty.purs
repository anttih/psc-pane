module PscPane.Pretty where

import Prelude

import Data.Array (length, take, filter)
import Data.Either (Either(Right))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), joinWith, split, trim)
import Data.String.Regex as R
import Data.String.Regex.Flags (RegexFlags(..), global)
import Node.Path (FilePath, relative)
import PscIde.Command (RebuildError(..), RangePosition)
import PscPane.Color (green, yellow, red)
import PscPane.Spawn (SpawnOutput)
import PscPane.State (State(..), Progress(InProgress, Done), PscFailure(Warning, Error))

type Height = Int

showProgress ∷ Progress → String
showProgress (InProgress progress) = "(" <> progress <> ")"
showProgress Done = ""

formatState ∷ Boolean → FilePath → Height → State → String
formatState _ _ _ InitialBuild = "Building project..."
formatState colorize cwd height (PscError res) = pretty colorize cwd height res
formatState colorize _ _ (CompilingModule path ) = "Compiling " <> path
formatState colorize _ _ (ModuleOk path progress) =
  green' colorize "Module OK" <> " " <> path <> " " <> showProgress progress
formatState colorize _ _ (BuildSuccess progress) =
  green' colorize "Build successful " <> showProgress progress
formatState colorize _ height (TestFailure output) = red' colorize "Test failure" <>
                                                "\n" <> formatTestOutput height output
formatState colorize _ _ TestSuccess = green' colorize "All tests pass"

pretty ∷ Boolean → FilePath → Height → PscFailure → String
pretty colorize cwd h (Warning warn) = prettyError' (yellow' colorize "Warning") cwd h warn
pretty colorize cwd h (Error err) = prettyError' (red' colorize "Error") cwd h err

yellow' ∷ Boolean → String → String
yellow' true = yellow
yellow' fale = identity

green' ∷ Boolean → String → String
green' true = green
green' fale = identity

red' ∷ Boolean → String → String
red' true = red
red' fale = identity

prettyError' ∷ String → FilePath → Height → RebuildError → String
prettyError' t cwd h (RebuildError err@{ position }) =
  t <> " " <> (filenameOrModule cwd err) <> (prettyPosition position)
  <> "\n" <> (prettyMessage (h - 1) (splitLines err.message))

filenameOrModule ∷ forall xs
  . FilePath
  → { filename ∷ Maybe String , moduleName ∷ Maybe String | xs }
  → String
filenameOrModule cwd { filename: Just file } = relative cwd file
filenameOrModule _ { moduleName: Just moduleName } = moduleName
filenameOrModule _ _ = ""

prettyPosition ∷ Maybe RangePosition → String
prettyPosition (Just { startLine, startColumn }) =
  " line " <> (show startLine) <> ", column " <> (show startColumn)
prettyPosition Nothing = ""

prettyMessage ∷ Height → Array String → String
prettyMessage height lines =
  let
    -- | Our fitting strategy
    tried ∷ Maybe (Array String)
    tried = try ( identity
                : trimLines
                : withoutExtraLines
                : withoutEmptyLines
                : Nil
                ) lines
    -- | Either we can fit the message intelligently or we just give up and
    -- | force the height
    formatted = maybe lines identity tried

  in joinLines formatted

  where

  trimLines ∷ Array String → Array String
  trimLines lines' = replace (R.regex "^\n+|\n+$" global) "" lines'


  withoutExtraLines ∷ Array String → Array String
  withoutExtraLines lines' = replace (R.regex "\n{2}" flags) "\n" lines'
    where flags = RegexFlags
                    { global: true
                    , ignoreCase: false
                    , multiline: true
                    , sticky: false
                    , unicode: false
                    }

  replace ∷ Either String R.Regex → String → Array String → Array String
  replace (Right regex) s lines' = splitLines (R.replace regex s (joinLines lines'))
  replace _ _ lines' = lines'

  withoutEmptyLines ∷ Array String → Array String
  withoutEmptyLines = filter ((_ /= "") <<< trim)

  -- | Try different fitting functions passing the previous result to the next
  -- | until the message fits.
  try ∷ List (Array String → Array String) → Array String → Maybe (Array String)
  try Nil _ = Nothing
  try (Cons f rest) lines' =
    let res = f lines'
    in if (length res) <= height
      then Just res
      else try rest res

formatTestOutput ∷ Height → SpawnOutput → String
formatTestOutput height { stdOut, stdErr } | stdErr == "" = takeLines height stdOut
formatTestOutput height { stdErr } = takeLines height stdErr

splitLines ∷ String → Array String
splitLines = split (Pattern "\n")

joinLines ∷ Array String → String
joinLines = joinWith "\n"

takeLines ∷ Height → String → String
takeLines height = joinLines <<< take height <<< splitLines

