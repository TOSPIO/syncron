module Syncron.Protocol.LinuxLocal where

import Syncron.Protocol (Protocol)
import Syncron.Path (LinuxPath)

data LinuxProtocol

instance Protocol LinuxLocal where
  type ProtocolPath proto = LinuxPath
  read = undefined
  write = undefined
