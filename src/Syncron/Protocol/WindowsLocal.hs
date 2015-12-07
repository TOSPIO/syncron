module Syncron.Protocol.WindowsLocal where

import Syncron.Protocol (Protocol)
import Syncron.Path (WindowsPath)

data WindowsProtocol

instance Protocol WindowsLocal where
  type ProtocolPath proto = WindowsPath
  read = undefined
  write = undefined
