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
  absolutize,
  getSubDirs,
  joinPath,
  ) where

import Control.Monad
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import System.Directory (doesDirectoryExist, getDirectoryContents, getHomeDirectory)
import System.FilePath ((</>), joinPath, addTrailingPathSeparator, normalise)
import System.Path.NameManip (guess_dotdot, absolute_path)


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
