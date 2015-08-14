{-# LANGUAGE ViewPatterns #-}
{- |
Module      :  Watcher
Description :  A simple tool that synchronizes two folders.
               Based on inotify and rsync.
Copyright   :  (c) TOSPIO
License     :  BSD v3

Maintainer  :  newelevenken@163.com
Stability   :  experimental
Portability :  non-portable

The watcher module
-}

module Watcher (
  runWatcher
               ) where

import Control.Concurrent (threadDelay)
import Control.Monad
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.List
import FileSystem
import Utils
import System.INotify
import System.IO.Unsafe
import System.Process

eventVarieties :: [EventVariety]
eventVarieties = [Modify, Attrib, Create, Delete, MoveIn, MoveOut]

{-# NOINLINE baseSrcDirRef #-}
baseSrcDirRef :: IORef FilePath
baseSrcDirRef = unsafePerformIO $ newIORef ""

{-# NOINLINE baseDstDirRef #-}
baseDstDirRef :: IORef FilePath
baseDstDirRef = unsafePerformIO $ newIORef ""

{-# NOINLINE dirWDMapperRef #-}
dirWDMapperRef :: IORef (HM.HashMap FilePath WatchDescriptor)
dirWDMapperRef = unsafePerformIO $ newIORef HM.empty

{-# NOINLINE excludedPatternsRef #-}
excludedPatternsRef :: IORef [String]
excludedPatternsRef = unsafePerformIO $ newIORef []

sync :: FilePath -> IO ()
sync relPath = do
  baseSrcDir <- readIORef baseSrcDirRef
  baseDstDir <- readIORef baseDstDirRef
  excludedPatterns <- readIORef excludedPatternsRef
  let srcDir = normalize $ joinPath [baseSrcDir, relPath]
  let dstDir = normalize $ joinPath [baseDstDir, relPath]
  putStrLn $ "Syncing " ++ srcDir ++ " to " ++ dstDir
  normalizedContents <- getNormalizedContents srcDir
  excludedContents <- getExcludedContents baseSrcDir srcDir normalizedContents excludedPatterns
  let slashedExcludedContents = map ("/"++) excludedContents
  case normalizedContents of
    [] -> return ()
    otherwise -> do
      let cmdArgs = ["-lptgodDvz"] ++
                    map (joinPath2 srcDir) normalizedContents ++
                    [dstDir] ++
                    case excludedPatterns of
                    [] -> []
                    otherwise ->
                      map ("--exclude=" ++) $ slashedExcludedContents
      print cmdArgs
      callProcess "rsync" cmdArgs

disappearEventView :: Event -> Maybe Event
disappearEventView evt
  | (Deleted {}) <- evt = Just evt
  | (MovedOut {}) <- evt = Just evt
  | otherwise = Nothing

appearEventView :: Event -> Maybe Event
appearEventView evt
  | (Created {}) <- evt = Just evt
  | (MovedIn {}) <- evt = Just evt
  | otherwise = Nothing

adjustINotify :: INotify -> FilePath -> Event -> IO ()
adjustINotify inotify srcDir (appearEventView -> Just evt) =
  when (isDirectory evt) $ do
    let path = filePath evt
    let extendedPath = joinPath [srcDir, path]
    watch inotify extendedPath
adjustINotify _ _ _ = return ()


handleEvent :: INotify -> FilePath -> Event -> IO ()

handleEvent _ relPath (disappearEventView -> Just evt) = do
  baseDstDir <- readIORef baseDstDirRef
  let path = filePath evt
  let extendedPath = joinPath [relPath, path]
  let fullPath = joinPath [baseDstDir, extendedPath]
  callProcess "rm" ["-rf", fullPath]
  wd <- unmapWatch extendedPath
  removeWatch wd

handleEvent inotify relPath evt = do
  let path = filePath evt
  adjustINotify inotify relPath evt
  sync relPath
  -- if not $ isDirectory evt
  --   then
  --   sync relPath
  --   else
  --   let extendedPath = joinPath [relPath, path] in
  --   sync extendedPath

watch :: INotify -> FilePath -> IO ()
watch inotify relPath = do
  baseSrcDir <- readIORef baseSrcDirRef
  let absolutePath = if relPath == "." then baseSrcDir else joinPath [baseSrcDir, relPath]
  putStrLn ("Adding " ++ absolutePath ++ " to watch list")
  wd <- addWatch inotify eventVarieties absolutePath (handleEvent inotify relPath)
  mapWatch relPath wd

mapWatch :: FilePath -> WatchDescriptor -> IO ()
mapWatch relPath wd = do
  dirWDMapper <- readIORef dirWDMapperRef
  writeIORef dirWDMapperRef $ HM.insert relPath wd dirWDMapper

unmapWatch :: FilePath -> IO WatchDescriptor
unmapWatch relPath = do
  dirWDMapper <- readIORef dirWDMapperRef
  let wd = HM.lookup relPath dirWDMapper
  writeIORef dirWDMapperRef $ HM.delete relPath dirWDMapper
  case wd of
    (Just wd') -> return wd'
    _ -> error $ "Unexpected missing of watch descriptor for path " ++ relPath

runWatcher :: FilePath -> FilePath -> Bool -> [FilePath] -> IO ()
runWatcher srcDir dstDir noStartupSync excludedPatterns = do
  absoluteSrcDir <- makeAbsolute srcDir
  absoluteDstDir <- makeAbsolute dstDir
  writeIORef baseSrcDirRef absoluteSrcDir
  writeIORef baseDstDirRef absoluteDstDir
  writeIORef excludedPatternsRef excludedPatterns
  withINotify $ \inotify -> do
    subDirs <- getSubDirs absoluteSrcDir absoluteSrcDir excludedPatterns
    forM_ subDirs $ \subDir -> do
      let relPath = makeRelative absoluteSrcDir subDir
      watch inotify relPath
      unless noStartupSync $ sync relPath
      return ()
    forever $ threadDelay 3000000
