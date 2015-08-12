{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module WildMatch (wildmatch) where

import Data.ByteString
import Data.IntMap.Lazy
import Foreign
import Foreign.C.Types
import Foreign.C.String


data MatchResult = WMMatch | WMNoMatch | WMAbortMalformed | WMAbortAll | WMAbortToStarStar
resultMap :: IntMap MatchResult
resultMap = fromList [
  (-2, WMAbortToStarStar),
  (-1, WMAbortAll),
  (0, WMMatch),
  (1, WMNoMatch),
  (2, WMAbortMalformed)
  ]

foreign import ccall "wildmatch.h wildmatch"
  c_wildmatch :: CString -> CString -> CUInt -> Ptr a -> Int
wildmatch :: ByteString -> ByteString -> Int -> IO MatchResult
wildmatch pattern text flags = do
  c_pattern <- useAsCString pattern
  c_text <- useAsCString text
  return $ c_wildmatch c_pattern c_text flags nullPtr
