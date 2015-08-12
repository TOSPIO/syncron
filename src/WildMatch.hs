{-# LINE 1 "WildMatch.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "WildMatch.hsc" #-}

module WildMatch where

import Foreign
import Foreign.C.Types
import Foreign.C.String


{-# LINE 10 "WildMatch.hsc" #-}

data WildOpts = WildOpts
instance Storable WildOpts where
  sizeOf _ = 0
  alignment = sizeOf
