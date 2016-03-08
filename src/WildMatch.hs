{-# LINE 1 "src/WildMatch.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "src/WildMatch.hsc" #-}

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


{-# LINE 19 "src/WildMatch.hsc" #-}

newtype MatchFlag =
  MatchFlag { unwrapMatchFlag :: CInt } deriving (Eq, Show)

wmNothing  :: MatchFlag
wmNothing  = MatchFlag 0
wmCaseFold  :: MatchFlag
wmCaseFold  = MatchFlag 1
wmPathName  :: MatchFlag
wmPathName  = MatchFlag 2

{-# LINE 28 "src/WildMatch.hsc" #-}

newtype MatchResult =
  MatchResult { unwrapMatchResult :: CInt } deriving (Eq, Show)

wmAbortToStarStar  :: MatchResult
wmAbortToStarStar  = MatchResult (-2)
wmAbortAll  :: MatchResult
wmAbortAll  = MatchResult (-1)
wmMatch  :: MatchResult
wmMatch  = MatchResult 0
wmNoMatch  :: MatchResult
wmNoMatch  = MatchResult 1
wmAbortMalformed  :: MatchResult
wmAbortMalformed  = MatchResult 2

{-# LINE 39 "src/WildMatch.hsc" #-}

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
