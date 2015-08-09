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

import Data.List.Utils
import Options.Applicative
import Watcher

data Args = Args {
  srcDir :: String,
  dstDir :: String,
  noStartUpSync :: Bool,
  excludedDirs :: Maybe String
  }

argParser :: Parser Args
argParser =
  Args
  <$> argument str (metavar "SRCDIR") -- Positional
  <*> argument str (metavar "DSTDIR") -- Positional
  <*> switch ( -- True or False
    long "no-startup-sync"
    <> short 'n'
    <> help "Do not sync on startup. Only sync on changes"
             )
  <*> optional ( -- Optional
    strOption $
    long "exclude"
    <> metavar "EXCLUDED_DIRS"
    <> help "The paths to exclude from being watched"
    )

run :: Args -> IO ()
run (Args {
         srcDir=srcDir0,
         dstDir=dstDir0,
         noStartUpSync=noStartUpSync0,
         excludedDirs=excludedDirs0
         }) =
  runWatcher srcDir0 dstDir0 noStartUpSync0 splitExcludedDirs
  where splitExcludedDirs =
          case excludedDirs0 of
          Just justExcludedDirs -> split "," justExcludedDirs
          Nothing -> []

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> argParser)
           $ fullDesc
           <> progDesc "Synchronize SRCDIR to DSTDIR"
           <> header "Syncron - a simple tool that synchronizes two folders real-time"
