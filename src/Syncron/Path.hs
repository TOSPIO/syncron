module Syncron.Path (
  Path,
  module Syncron.Path.WindowsPath,
  module Syncron.Path.PosixPath,
  ) where

import Syncron.Path.WindowsPath
import Syncron.Path.PosixPath

class Path a
