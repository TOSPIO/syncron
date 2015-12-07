{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Syncron.Protocol (
  Protocol,
  module Syncron.Protocol.LinuxLocal,
  module Syncron.Protocol.WindowsLocal
  ) where

import Syncron.Core (Data)
import Syncron.Path (Path (..))
import Syncron.Protocol.LinuxLocal
import Syncron.Protocol.WindowsLocal

class Protocol proto where
  type ProtocolPath proto :: *
  type ProtocolPath proto = forall p. (Path p) => p
  read :: ProtocolPath proto -> Data a
  write :: Data a -> ProtocolPath proto
