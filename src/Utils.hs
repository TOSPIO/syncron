{- |
Module      :  Utils
Description :  A simple tool that synchronizes two folders.
               Based on inotify and rsync.
Copyright   :  (c) TOSPIO
License     :  BSD v3

Maintainer  :  newelevenken@163.com
Stability   :  experimental
Portability :  non-portable

The utils module
-}

module Utils (
  anyM
  ) where

import Control.Monad

-- TODO: change [a] to Foldable t => t a
anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM m t = case t of
  [] -> return False
  (x:xs) -> do
    result <- m x
    if result then return True else anyM m xs
