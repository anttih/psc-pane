module PscPane.Server where

import Prelude
import Data.Maybe (Maybe(..), isNothing)
import PscIde.Server (ServerStartResult(..), startServer')
import PscPane.Types (AffN)
import Data.List (List(Cons, Nil))
import Node.Process (stderr)
import Node.ChildProcess (StdIOBehaviour(Ignore, ShareStream))
import Node.Stream (Stream)
import Unsafe.Coerce (unsafeCoerce)

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
    serverRunning port <$> startServer' stdio "psc-ide-server" port (Just dir)
      where
      stdio = map Just [ Ignore
                       , Ignore
                       , ShareStream (unsafeCoerce stderr :: forall r eff. Stream r eff)
                       ]


serverRunning ∷ Int -> ServerStartResult → Maybe Int
serverRunning port (Started _) = Just port
serverRunning _ _ = Nothing

