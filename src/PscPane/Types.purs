module PscPane.Types where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.Process (PROCESS)
import PscIde (NET)

type EffN = Eff ( fs :: FS
                , err :: EXCEPTION
                , console :: CONSOLE
                , buffer :: BUFFER
                , process :: PROCESS
                , cp :: CHILD_PROCESS
                , avar :: AVAR
                , net :: NET
                )

type AffN = Aff ( fs :: FS
                , err :: EXCEPTION
                , console :: CONSOLE
                , buffer :: BUFFER
                , process :: PROCESS
                , cp :: CHILD_PROCESS
                , avar :: AVAR
                , net :: NET
                )
