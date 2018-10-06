module PscPane.Server where

import Prelude

import Data.List (List(Cons, Nil))
import Data.Maybe (Maybe(..), isNothing)
import Effect.Aff (Aff)
import Node.ChildProcess (ignore)
import PscIde.Server (ServerStartResult(..), defaultServerArgs, startServer)

startPscIdeServer ∷ String → List Int → Aff (Maybe Int)
startPscIdeServer dir ports = go ports
  where
  go ∷ List Int → Aff (Maybe Int)
  go Nil = pure Nothing
  go (Cons port ports') = do
    res ← startOnPort port
    if isNothing res then go ports' else pure res

  startOnPort ∷ Int → Aff (Maybe Int)
  startOnPort port =
    let options = (_ { stdio = ignore, port = Just port, directory = Just dir }) defaultServerArgs
    in serverRunning port <$> startServer options

serverRunning ∷ Int → ServerStartResult → Maybe Int
serverRunning port (Started _) = Just port
serverRunning _ _ = Nothing

