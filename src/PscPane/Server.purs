module PscPane.Server where

import Prelude
import Data.Maybe (Maybe(..), isNothing)
import PscIde.Server (ServerStartResult(..), startServer)
import PscPane.Types (AffN)
import Data.List (List(Cons, Nil))

startPscIdeServer :: String -> List Int -> AffN (Maybe Int)
startPscIdeServer dir ports = go ports
  where
  go :: List Int -> AffN (Maybe Int)
  go Nil = pure Nothing
  go (Cons port ports') = do
    res <- startOnPort port
    if isNothing res then go ports' else pure res

  startOnPort :: Int -> AffN (Maybe Int)
  startOnPort port =
    serverRunning port <$> startServer "psc-ide-server" port (Just dir)


serverRunning ∷ Int -> ServerStartResult → Maybe Int
serverRunning port (Started _) = Just port
serverRunning _ _ = Nothing

