{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
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

import Control.Concurrent (threadDelay)
import Control.Monad
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)
import Data.IORef
import System.Directory (doesDirectoryExist, getDirectoryContents, getHomeDirectory)
import System.Environment
import System.FilePath ((</>), joinPath, addTrailingPathSeparator, normalise, isAbsolute)
import System.INotify
import System.IO.Unsafe
import System.Path.NameManip (guess_dotdot, absolute_path)
import System.Process

absolutize :: String -> IO String
absolutize aPath
    | "~" `isPrefixOf` aPath = do
        homePath <- getHomeDirectory
        return $ normalise $ addTrailingPathSeparator homePath
                             ++ tail aPath
    | otherwise = do
        pathMaybeWithDots <- absolute_path aPath
        return $ fromJust $ guess_dotdot pathMaybeWithDots

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

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ usageStr progName
    where usageStr progName = "usage: " ++ progName ++ " src dst"

sync :: FilePath -> IO ()
sync relPath = do
  baseSrcDir <- readIORef baseSrcDirRef
  baseDstDir <- readIORef baseDstDirRef
  let srcDir = joinPath [baseSrcDir, relPath]
  let dstDir = joinPath [baseDstDir, relPath]
  putStrLn dstDir
  putStrLn $ "Syncing " ++ srcDir ++ " to " ++ dstDir
  callProcess "rsync" ["-av", srcDir ++ "/", dstDir ++ "/"]

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
  if not $ isDirectory evt
    then
    sync relPath
    else
    let extendedPath = joinPath [relPath, path] in
    sync extendedPath

watch :: INotify -> FilePath -> IO ()
watch inotify relPath = do
  putStrLn ("Adding " ++ relPath ++ " to watch list")
  baseSrcDir <- readIORef baseSrcDirRef
  let absolutePath = joinPath [baseSrcDir, relPath]
  wd <- addWatch inotify eventVarieties absolutePath (handleEvent inotify relPath)
  dirWDMapper <- readIORef dirWDMapperRef
  writeIORef dirWDMapperRef $ HM.insert relPath wd dirWDMapper

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

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
    then usage
    else
    let (srcDir, dstDir) = extractDirs args in do
      absoluteSrcDir <- absolutize srcDir
      absoluteDstDir <- absolutize dstDir
      writeIORef baseSrcDirRef absoluteSrcDir
      writeIORef baseDstDirRef absoluteDstDir
      withINotify $ \inotify -> do
        subDirs <- getSubDirs absoluteSrcDir
        forM_ subDirs $ \subDir -> do
          let relPath = drop (length absoluteSrcDir + 1) subDir
          putStrLn relPath
          watch inotify relPath
          sync relPath
          return ()
        forever $ threadDelay 3000000
  where
    extractDirs args
      | srcDir:dstDir:_ <- args = (srcDir, dstDir)
