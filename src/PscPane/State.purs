module PscPane.State
  ( State(..)
  , Progress(..)
  , PscFailure(..)
  ) where

import Node.Path (FilePath)
import PscIde.Command (RebuildError)

import PscPane.Spawn (SpawnOutput)

data Progress = InProgress String | Done

data PscFailure = Warning RebuildError | Error RebuildError

data State
  = InitialBuild
  | CompilingModule FilePath
  | BuildSuccess Progress
  | ModuleOk FilePath Progress
  | PscError PscFailure
  | TestFailure SpawnOutput
  | TestSuccess

