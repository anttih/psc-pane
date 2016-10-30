module PscPane.State
  ( State(..)
  , Progress(..)
  , PaneResult(..)
  ) where

import Node.Path (FilePath)
import PscIde.Command (RebuildError)

data Progress = InProgress String | Done

data PaneResult = Warning RebuildError | Error RebuildError

data State
  = InitialBuild
  | BuildSuccess Progress
  | ModuleOk FilePath Progress
  | PscError PaneResult
  | TestFailure String
  | TestSuccess

