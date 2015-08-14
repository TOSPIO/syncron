{- |
Module      :  FileSystem
Description :  A simple tool that synchronizes two folders.
               Based on inotify and rsync.
Copyright   :  (c) TOSPIO
License     :  BSD v3

Maintainer  :  newelevenken@163.com
Stability   :  experimental
Portability :  non-portable

The module that operates files.
-}

module FileSystem (
  makeAbsolute,
  makeRelative,
  getSubDirs,
  joinPath,
  joinPath2,
  normalize,
  getNormalizedContents,
  getExcludedContents,
  ) where

import Control.Monad
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import System.Directory (doesDirectoryExist, getDirectoryContents, getHomeDirectory)
import System.FilePath ((</>), joinPath, addTrailingPathSeparator, normalise)
import System.Path.NameManip (guess_dotdot, absolute_path)
import qualified WildMatch as WM

makeAbsolute :: String -> IO String
makeAbsolute aPath
    | "~" `isPrefixOf` aPath = do
        homePath <- getHomeDirectory
        return $ normalise $ addTrailingPathSeparator homePath
                             ++ tail aPath
    | otherwise = do
        pathMaybeWithDots <- absolute_path aPath
        return $ fromJust $ guess_dotdot pathMaybeWithDots

makeRelative :: String -> String -> String
makeRelative baseDir absolutePath =
  let nBaseDir = normalise baseDir
      nAbsolutePath = normalise absolutePath
  in
    case drop (length nBaseDir + 1) nAbsolutePath of
    "" -> "."
    p -> p

normalize = normalise

joinPath2 :: FilePath -> FilePath -> FilePath
joinPath2 l r = joinPath [l, r]

getNormalizedContents :: FilePath -> IO [FilePath]
getNormalizedContents topDir = do
  names <- getDirectoryContents topDir
  let normalNames = filter (\x -> x /= "." && x /= "..") names
  return normalNames

getSubDirs :: FilePath -> FilePath -> [String] -> IO [FilePath]
getSubDirs baseDir topDir excludedPatterns = do
  normalNames <- getNormalizedContents topDir
  let relativePaths = map ((makeRelative baseDir). (joinPath2 topDir)) normalNames
  properNames <- filterM (\p -> do
                              result <- WM.wildmatchAny excludedPatterns p
                              return $ not result
                         ) $ relativePaths
  paths <- forM properNames $ \name -> do
    let path = baseDir </> name
    isDir <- doesDirectoryExist path
    if isDir
      then
      getSubDirs baseDir path excludedPatterns
      else return []
  return $ topDir:concat paths

getExcludedContents :: FilePath -> FilePath -> [FilePath] -> [String] -> IO [FilePath]
getExcludedContents baseDir topDir names excludedPatterns = do
  -- the `topDir` is a relative path that app excludedPatterns starts at
  case excludedPatterns of
    [] -> return names
    otherwise -> filterM (\name -> do
                              let relativeName = makeRelative baseDir $ joinPath2 topDir name
                              WM.wildmatchAny excludedPatterns relativeName
                              ) names

