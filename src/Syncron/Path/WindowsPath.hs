module Syncron.Path.WindowsPath (
  WindowsPath (..)
  ) where

import Syncron.Path (Path)

newtype WindowsPath = WindowsPath String

instance Path WindowsPath
