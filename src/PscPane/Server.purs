module PscPane.Server where

import Prelude
import Data.Maybe (Maybe(..))
import PscIde.Server (ServerStartResult(..), startServer)
import PscPane.Types (AffN)

startPscIdeServer :: String -> AffN (Maybe Int)
startPscIdeServer dir =
  let port = 4040
  in serverRunning port <$> startServer "psc-ide-server" port (Just dir)

serverRunning ∷ Int -> ServerStartResult → Maybe Int
serverRunning port (Started _) = Just port
serverRunning _ _ = Nothing

