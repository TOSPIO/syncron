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

import System.Environment
import System.FilePath ((</>), joinPath, addTrailingPathSeparator, normalise, isAbsolute)
import System.Directory (doesDirectoryExist, getDirectoryContents, getHomeDirectory)
import System.INotify
import System.Process
import System.IO.Unsafe
import Control.Concurrent (threadDelay)
import Control.Monad
import System.Path.NameManip (guess_dotdot, absolute_path)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)
import Data.IORef

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
eventVarieties = [Modify, Attrib, Create, Delete, DeleteSelf, MoveSelf]


{-# NOINLINE baseSrcDirRef #-}
baseSrcDirRef :: IORef FilePath
baseSrcDirRef = unsafePerformIO $ newIORef ""

{-# NOINLINE baseDstDirRef #-}
baseDstDirRef :: IORef FilePath
baseDstDirRef = unsafePerformIO $ newIORef ""

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

adjustINotify :: INotify -> FilePath -> Event -> IO ()
adjustINotify inotify srcDir (Created {isDirectory=True, filePath=path}) = do
  let extendedPath = joinPath [srcDir, path]
  watch inotify extendedPath
adjustINotify _ _ _ = return ()

deleteEventView :: Event -> Maybe Event
deleteEventView evt
  | DeletedSelf <- evt = Just evt
  | (Deleted _ _) <- evt = Just evt
  | otherwise = Nothing

handleEvent :: INotify -> FilePath -> Event -> IO ()

handleEvent _ relPath (deleteEventView -> Just evt) = do
  let path = filePath evt
  baseDstDir <- readIORef baseDstDirRef
  let fullPath = joinPath [baseDstDir, relPath, path]
  callProcess "rm" ["-rf", fullPath]

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
  return ()
