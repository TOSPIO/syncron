module Syncron.Path.PosixPath (
  PosixPath (..)
  ) where

import Syncron.Path (Path)

newtype PosixPath = PosixPath String

instance Path PosixPath
