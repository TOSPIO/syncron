{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module WildMatch (
  wildmatch,
  wildmatchAny,
  fastSin,
  MatchResult(..),
  wmMatch,
  ) where

import Data.ByteString.Char8
import Data.IntMap.Lazy
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Utils

#include "wildmatch/wildmatch.h"

newtype MatchFlag =
  MatchFlag { unwrapMatchFlag :: CInt } deriving (Eq, Show)

#{enum MatchFlag, MatchFlag,
  wmNothing = 0,
  wmCaseFold = WM_CASEFOLD,
  wmPathName = WM_PATHNAME
 }

newtype MatchResult =
  MatchResult { unwrapMatchResult :: CInt } deriving (Eq, Show)

#{enum MatchResult, MatchResult,
  wmAbortToStarStar = WM_ABORT_TO_STARSTAR,
  wmAbortAll = WM_ABORT_ALL,
  wmMatch = WM_MATCH,
  wmNoMatch = WM_NOMATCH,
  wmAbortMalformed = WM_ABORT_MALFORMED
 }

foreign import ccall "wildmatch.h wildmatch"
  c_wildmatch :: CString -> CString -> CUInt -> Ptr a -> CInt

-- Always use wmPathName as MatchFlag
wildmatch :: String -> String -> IO Bool
wildmatch pattern text = do
  let bsPattern = pack pattern
  let bsText = pack text
  useAsCString bsPattern $ \c_pattern -> do
    useAsCString bsText $ \c_text -> do
      let c_ret = c_wildmatch c_pattern c_text (fromIntegral $ unwrapMatchFlag wmPathName) nullPtr
      return $ (MatchResult $ fromIntegral c_ret) == wmMatch

wildmatchAny :: [String] -> String -> IO Bool
wildmatchAny [] text = return True
wildmatchAny patterns text = anyM (\p -> wildmatch p text) patterns

-- For FFI test purpose
foreign import ccall "math.h sin"
  c_sin :: CDouble -> CDouble
fastSin :: Double -> Double
fastSin = realToFrac . c_sin . realToFrac
