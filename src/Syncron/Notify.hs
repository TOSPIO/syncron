module Syncron.Notify where

import Syncron.Core (IPath)

data Path path = (IPath path) => Path path

data SyncronEvent = Modify path
                  | Create
                  | Delete
                  | MoveIn
                  | MoveOut

newtype SyncronEventHandler = SycronEventHandler { handler :: SyncronEvent -> IO a }

class INotify where
  register :: (IPath path) => path 
