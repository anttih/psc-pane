module Pretty where

import Prelude ((<>), show)
import Data.Array (head, take)
import Data.String (joinWith)
import Data.Maybe (Maybe(..), maybe)

import Parse

type Height = Int

pretty :: PscResult -> String
pretty (PscResult { warnings: [], errors: [] }) = "All good"
pretty (PscResult { warnings: [], errors: errors }) = maybe "" (prettyError 5) (head errors)
pretty (PscResult { warnings: warnings, errors: [] }) = maybe "" (prettyWarning 5) (head warnings)
pretty (PscResult { warnings: _, errors: errors }) = maybe "" (prettyError 5) (head errors)

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
prettyMessage height lines = joinWith "\n" (take height lines)
