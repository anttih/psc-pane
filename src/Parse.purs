module Parse where

import Prelude (($), bind, pure)
import Data.Maybe (Maybe)
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Foreign.Null (runNull)
import Data.String (split)

type ErrorCode = String

newtype Position = Position
  { startLine :: Int
  , endLine :: Int
  , startColumn :: Int
  , endColumn :: Int
  }

instance isForeignPosition :: IsForeign Position where
  read d = do
    startLine <- readProp "startLine" d
    endLine <- readProp "endLine" d
    startColumn <- readProp "startColumn" d
    endColumn <- readProp "endColumn" d
    pure $ Position { startLine, endLine, startColumn, endColumn }

newtype PscError = PscError
  { moduleName :: Maybe String
  , errorCode :: ErrorCode
  , message :: Array String
  , filename :: Maybe String
  , position :: Maybe Position
  }

instance isForeignPscError :: IsForeign PscError where
  read d = do
    moduleName <- readProp "moduleName" d
    errorCode <- readProp "errorCode" d
    message <- readProp "message" d
    filename <- readProp "filename" d
    position <- readProp "position" d
    pure $ PscError { moduleName: runNull moduleName
                    , errorCode: errorCode
                    , message: parseMessage message
                    , position: runNull position
                    , filename: runNull filename
                    }

newtype PscResult = PscResult
  { warnings :: Array PscError
  , errors :: Array PscError
  }

instance isForeignPscResult :: IsForeign PscResult where
  read d = do
    warnings <- readProp "warnings" d
    errors <- readProp "errors" d
    pure $ PscResult { warnings, errors }

parseMessage :: String -> Array String
parseMessage = split "\n"
