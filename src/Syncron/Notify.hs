{-# LANGUAGE GADTs #-}

module Syncron.Notify (
  SyncronEvent (..),
  INotify,
  module Syncron.Notify.Inotify
  ) where

import Syncron.Path (Path)
import Syncron.Notify.Inotify

data SyncronEvent where
  Modify :: (Path p) => p -> SyncronEvent
  Create :: (Path p) => p -> SyncronEvent
  Delete :: (Path p) => p -> SyncronEvent
  MoveIn :: (Path pf, Path pt) => pf -> pt -> SyncronEvent
  MoveOut :: (Path pf, Path pt) => pf -> pt -> SyncronEvent

newtype SyncronEventHandler a = SycronEventHandler {
  handler :: SyncronEvent -> IO a
  }

class INotify a where
  register :: (Path p) => p -> SyncronEventHandler a -> IO b
