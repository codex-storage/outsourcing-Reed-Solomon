
{-# LANGUAGE StrictData, ForeignFunctionInterface #-}

module NTT.FFT.Fast where

--------------------------------------------------------------------------------

import Data.Word

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal

import System.IO.Unsafe

import Data.Flat

import NTT.Poly
import NTT.Subgroup
import Field.Goldilocks
import Misc

--------------------------------------------------------------------------------

{-
void goldilocks_ntt_forward           (              int m, uint64_t gen, const uint64_t *src, uint64_t *tgt);
void goldilocks_ntt_forward_shifted   (uint64_t eta, int m, uint64_t gen, const uint64_t *src, uint64_t *tgt);

void goldilocks_ntt_forward_asymmetric(int m_src, int m_tgt, uint64_t gen_src, uint64_t gen_tgt, const uint64_t *src, uint64_t *tgt);

void goldilocks_ntt_inverse           (              int m, uint64_t gen, const uint64_t *src, uint64_t *tgt);
void goldilocks_ntt_inverse_shifted   (uint64_t eta, int m, uint64_t gen, const uint64_t *src, uint64_t *tgt);
-}

foreign import ccall unsafe "goldilocks_ntt_forward"         c_ntt_forward         ::           CInt -> Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "goldilocks_ntt_forward_shifted" c_ntt_forward_shifted :: Word64 -> CInt -> Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

foreign import ccall unsafe "goldilocks_ntt_forward_asymmetric" c_ntt_forward_asymmetric :: CInt -> CInt -> Word64 -> Word64 -> Ptr Word64 -> Ptr Word64 -> IO ();

foreign import ccall unsafe "goldilocks_ntt_inverse"         c_ntt_inverse         ::           CInt -> Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "goldilocks_ntt_inverse_shifted" c_ntt_inverse_shifted :: Word64 -> CInt -> Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

--------------------------------------------------------------------------------

{-# NOINLINE forwardNTT #-}
forwardNTT :: Subgroup F -> Poly F -> FlatArray F
forwardNTT sg (MkPoly (MkFlatArray n fptr2))
  | subgroupSize sg /= n   = error "forwardNTT: subgroup size differs from the array size"
  | otherwise              = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray n 
      let MkGoldilocks gen = subgroupGen sg
      withForeignPtr fptr2 $ \ptr2 -> do
        withForeignPtr fptr3 $ \ptr3 -> do
          c_ntt_forward (subgroupCLogSize sg) gen ptr2 ptr3
      return (MkFlatArray n fptr3)

{-# NOINLINE inverseNTT #-}
inverseNTT :: Subgroup F -> FlatArray F -> Poly F
inverseNTT sg (MkFlatArray n fptr2)
  | subgroupSize sg /= n   = error "inverseNTT: subgroup size differs from the array size"
  | otherwise              = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray n 
      let MkGoldilocks gen = subgroupGen sg
      withForeignPtr fptr2 $ \ptr2 -> do
        withForeignPtr fptr3 $ \ptr3 -> do
          c_ntt_inverse (subgroupCLogSize sg) gen ptr2 ptr3
      return (MkPoly (MkFlatArray n fptr3))

-- | Pre-multiplies the coefficients by powers of eta, effectively evaluating @f(eta*x)@ on the subgroup
{-# NOINLINE shiftedForwardNTT #-}
shiftedForwardNTT :: Subgroup F -> F -> Poly F -> FlatArray F
shiftedForwardNTT sg (MkGoldilocks eta) (MkPoly (MkFlatArray n fptr2))
  | subgroupSize sg /= n   = error "shiftedForwardNTT: subgroup size differs from the array size"
  | otherwise              = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray n 
      let MkGoldilocks gen = subgroupGen sg
      withForeignPtr fptr2 $ \ptr2 -> do
        withForeignPtr fptr3 $ \ptr3 -> do
          c_ntt_forward_shifted eta (subgroupCLogSize sg) (subgroupGenAsWord64 sg) ptr2 ptr3
      return (MkFlatArray n fptr3)

-- | Post-multiplies the coefficients by powers of eta, effectively interpolating @f@ such that @f(eta^-1 * omega^k) = y_k@
{-# NOINLINE shiftedInverseNTT #-}
shiftedInverseNTT :: Subgroup F -> F -> FlatArray F -> Poly F
shiftedInverseNTT sg (MkGoldilocks eta) (MkFlatArray n fptr2)
  | subgroupSize sg /= n   = error "shiftedInverseNTT: subgroup size differs from the array size"
  | otherwise              = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray n 
      withForeignPtr fptr2 $ \ptr2 -> do
        withForeignPtr fptr3 $ \ptr3 -> do
          c_ntt_inverse_shifted eta (subgroupCLogSize sg) (subgroupGenAsWord64 sg) ptr2 ptr3
      return (MkPoly (MkFlatArray n fptr3))

-- | Evaluates a polynomial f on a larger subgroup than it's defined on
{-# NOINLINE asymmForwardNTT #-}
asymmForwardNTT :: Subgroup F -> Poly F -> Subgroup F -> FlatArray F
asymmForwardNTT sg_src (MkPoly (MkFlatArray n fptr2)) sg_tgt
  | subgroupSize sg_src /= n   = error "asymmForwardNTT: subgroup size differs from the array size"
  | m < n                      = error "asymmForwardNTT: target subgroup size should be at least the source subgroup src"
  | otherwise                  = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray m 
      let MkGoldilocks sgen1 = subgroupGen sg_src
      let MkGoldilocks sgen2 = subgroupGen sg_tgt
      withForeignPtr fptr2 $ \ptr2 -> do
        withForeignPtr fptr3 $ \ptr3 -> do
          c_ntt_forward_asymmetric
            (subgroupCLogSize sg_src)
            (subgroupCLogSize sg_tgt)
            sgen1 sgen2 ptr2 ptr3
      return (MkFlatArray m fptr3)
  where
    m = subgroupSize sg_tgt

{-
instance P.UnivariateFFT Poly where
  ntt         = forwardNTT
  intt        = inverseNTT
  shiftedNTT  = shiftedForwardNTT
  shiftedINTT = shiftedInverseNTT
  asymmNTT    = asymmForwardNTT
-}

--------------------------------------------------------------------------------

subgroupCLogSize :: Subgroup a -> CInt
subgroupCLogSize = fromIntegral . fromLog2 . subgroupLogSize

subgroupGenAsWord64 :: Subgroup F -> Word64
subgroupGenAsWord64 sg = let MkGoldilocks x = subgroupGen sg in x

--------------------------------------------------------------------------------
