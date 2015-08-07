{-# LANGUAGE TypeFamilies #-}
{- |
Module      :  Main
Description :  A simple tool that synchronizes two folders.
               Based on inotify and rsync.
Copyright   :  (c) TOSPIO
License     :  BSD v3

Maintainer  :  newelevenken@163.com
Stability   :  experimental
Portability :  non-portable

The main module
-}

module Main where

import System.Environment
import Watcher


usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ usageStr progName
    where usageStr progName = "usage: " ++ progName ++ " src dst"

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
    then usage
    else
    let (srcDir, dstDir) = extractDirs args
    in
      runWatcher srcDir dstDir
  where
    extractDirs args
      | srcDir:dstDir:_ <- args = (srcDir, dstDir)
