module PscPane.Server
  ( ServerStartResult(..)
  , startServer
  , serverRunning
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Aff (later', makeAff)
import Control.Monad.Aff.Par (Par(Par), runPar)
import Control.Monad.Eff.Class (liftEff)
import Node.ChildProcess (Exit(Normally), onClose, onError, defaultSpawnOptions, spawn, ChildProcess)
import PscPane.Types (AffN)

data ServerStartResult = Started ChildProcess
                       | Closed
                       | StartError String

startServer :: String -> Int -> AffN ServerStartResult
startServer exe port = do
    cp <- liftEff (spawn exe ["-p", show port] defaultSpawnOptions)
    let handleErr = makeAff \_ succ -> do
                      onError cp (\_ -> succ $ StartError "psc-ide-server error")
                      onClose cp (\exit -> case exit of
                                     (Normally 0) -> succ Closed
                                     (Normally n) -> succ $ StartError $ "Error code returned: "++ show n
                                     _ -> succ $ StartError "Other close error")

    runPar (Par handleErr <|> Par (later' 100 $ pure $ Started cp))

serverRunning ∷ ServerStartResult → Boolean
serverRunning (Started _) = true
serverRunning _ = false
