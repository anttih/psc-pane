module PscPane.Action where

data ActionF a
  = RebuildModule String a
  | LoadModules a
  | BuildProject a
  | DisplayPaneState PaneState a
  
