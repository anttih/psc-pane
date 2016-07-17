module PscPane.Types where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.Process (PROCESS)
import PscIde (NET)
import Blessed (BLESSED)

type EffN = Eff ( fs :: FS
                , err :: EXCEPTION
                , console :: CONSOLE
                , buffer :: BUFFER
                , process :: PROCESS
                , cp :: CHILD_PROCESS
                , avar :: AVAR
                , net :: NET
                , ref :: REF
                , blessed :: BLESSED
                )

type AffN = Aff ( fs :: FS
                , err :: EXCEPTION
                , console :: CONSOLE
                , buffer :: BUFFER
                , process :: PROCESS
                , cp :: CHILD_PROCESS
                , avar :: AVAR
                , net :: NET
                , ref :: REF
                , blessed :: BLESSED
                )
