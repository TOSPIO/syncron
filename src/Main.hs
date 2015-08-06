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

import System.Environment
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.IO
import System.INotify
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Traversable (traverse)
import System.FilePath ((</>))

eventVarieties :: [EventVariety]
eventVarieties = [Modify, Attrib, Create, DeleteSelf, MoveSelf]

getSubDirs :: FilePath -> IO [FilePath]
getSubDirs topDir = do
  names <- getDirectoryContents topDir
  let properNames =
        let properNamePredicate ('.':_) = False
            properNamePredicate _ = True
        in
          filter properNamePredicate names
  paths <- forM properNames $ \name -> do
    let path = topDir </> name
    isDir <- doesDirectoryExist path
    if isDir
      then
        getSubDirs path
      else return []
  return $ topDir:concat paths

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
    then usage
    else
    let (srcDir, _) = extractDirs args in do
      subDirs <- getSubDirs srcDir
      watch subDirs
      return ()
  where
    extractDirs args
      | srcDir:dstDir:_ <- args = (srcDir, dstDir)

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ usageStr progName
    where usageStr progName = "usage: " ++ progName ++ " src dst"

handleEvent :: Event -> IO ()
handleEvent evt =
  let adjustINotify (Created {isDirectory=True, filePath=path}) = do
        print "OK"
        watch [path]
      adjustINotify _ = return ()
  in do
    print evt
    adjustINotify evt

watch :: [FilePath] -> IO ()
watch fs = withINotify $ \inotify -> do
        print fs
        mapM_ (\f -> addWatch inotify eventVarieties f handleEvent) fs
        forever $ threadDelay (10 * microsecsPerSec)
    where
        microsecsPerSec = 1000000
