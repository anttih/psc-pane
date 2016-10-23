module PscPane.Server where

import Prelude
import Data.Maybe (Maybe(..), isNothing)
import PscIde.Server (ServerStartResult(..), defaultServerArgs, startServer)
import PscPane.Types (AffN)
import Data.List (List(Cons, Nil))
import Node.ChildProcess (ignore)

startPscIdeServer ∷ String → List Int → AffN (Maybe Int)
startPscIdeServer dir ports = go ports
  where
  go ∷ List Int → AffN (Maybe Int)
  go Nil = pure Nothing
  go (Cons port ports') = do
    res ← startOnPort port
    if isNothing res then go ports' else pure res

  startOnPort ∷ Int → AffN (Maybe Int)
  startOnPort port =
    let options = (_ { stdio = ignore, port = Just port, directory = Just dir }) defaultServerArgs
    in serverRunning port <$> startServer options

serverRunning ∷ Int → ServerStartResult → Maybe Int
serverRunning port (Started _) = Just port
serverRunning _ _ = Nothing

