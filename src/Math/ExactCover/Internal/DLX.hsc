{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE EmptyDataDecls           #-}

-- |
-- = WARNING
--
-- This module is considered __internal__.
--

module Math.ExactCover.Internal.DLX where

import Foreign
import Foreign.C.Types


#include <dlx.h>

data DLXMatrix

foreign import ccall unsafe "dlx.h solve"
  c_solve :: Ptr DLXMatrix -> CSize -> Ptr (Ptr (Ptr CInt)) -> Ptr (Ptr CSize) -> Ptr CSize -> IO CInt

foreign import ccall unsafe "dlx.h &free_matrix"
  c_free_matrix :: FinalizerPtr DLXMatrix

foreign import ccall unsafe "dlx.h create_empty_matrix"
  c_create_empty_matrix :: IO (Ptr DLXMatrix)

foreign import ccall unsafe "dlx.h add_set"
  c_add_set :: Ptr (Ptr DLXMatrix) -> Ptr CInt -> CSize -> IO CInt
