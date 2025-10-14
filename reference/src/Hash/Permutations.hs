
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Hash.Permutations where

--------------------------------------------------------------------------------

import Data.Word

import Foreign.Ptr
import Foreign.ForeignPtr

import System.IO.Unsafe

import qualified Hash.Monolith.Permutation as Monolith

import Hash.Common
import Hash.State

--------------------------------------------------------------------------------

#ifdef USE_NAIVE_HASKELL

permute :: Hash -> State -> State
permute hash = case hash of
  Monolith -> Monolith.permutation

--------------------------------------------------------------------------------

#else

foreign import ccall unsafe "goldilocks_monolith_permutation"      c_monolith_permutation      :: Ptr Word64 -> IO ()
foreign import ccall unsafe "goldilocks_monolith_permutation_into" c_monolith_permutation_into :: Ptr Word64 -> Ptr Word64 -> IO ()

permuteInPlace :: State -> IO ()
permuteInPlace fptr = withForeignPtr fptr $ \ptr -> c_monolith_permutation (castPtr ptr)

{-# NOINLINE permuteIO #-}
permuteIO :: State -> IO State
permuteIO src = do
  tgt <- mallocForeignPtrArray 12
  withForeignPtr src $ \ptr1 ->
    withForeignPtr tgt $ \ptr2 ->
      c_monolith_permutation_into (castPtr ptr1) (castPtr ptr2)
  return tgt

permute :: Hash -> State -> State
permute _ what = unsafePerformIO (permuteIO what)

--------------------------------------------------------------------------------

#endif  

