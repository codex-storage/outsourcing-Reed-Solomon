
-- Short DFT algorithms (sizes 2, 4, 8 and 16)
--
-- See :
--
-- * Nussbaumer: "Fast Fourier Transform and Convolution Algorithms", Chapter 5.5
--

{-# LANGUAGE ForeignFunctionInterface #-}
module NTT.FFT.Short where

--------------------------------------------------------------------------------

import Data.Array
import Data.Word
import Data.List ( sort )

import Control.Monad

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import System.IO.Unsafe

import Class.Field
import Field.Goldilocks ( F , fromF )

import NTT.FFT.Slow
import NTT.Poly.Naive

import NTT.Subgroup
import Misc

--------------------------------------------------------------------------------
-- DFT mathematical definition - O(n^2)

defForwardDFT :: Subgroup F -> Array Int F -> Array Int F
defForwardDFT sg input
  | subgroupOrder sg /= n = error "defForwardDFT: input size does not match the subgroup"
  | otherwise             = listToArray [ f k | k <-[0..n-1] ]
  where
    n = arrayLength input
    omega = subgroupGen sg
    f k = sum [ input!i * power_ omega (k*i) | i<-[0..n-1] ] 

defInverseDFT :: Subgroup F -> Array Int F -> Array Int F
defInverseDFT sg input 
  | subgroupOrder sg /= n = error "defInverseDFT: input size does not match the subgroup"
  | otherwise             = listToArray [ f k / (fromIntegral n) | k <-[0..n-1] ]
  where
    n = arrayLength input
    omega = subgroupGen sg
    invomega = inverse omega
    f k = sum [ input!i * power_ invomega (k*i) | i<-[0..n-1] ] 

--------------------------------------------------------------------------------

foreign import ccall unsafe "short_fwd_DFT_size_4"          c_dft4_fwd          :: CInt -> CInt -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "short_inv_DFT_size_4_unscaled" c_dft4_inv_unscaled :: CInt -> CInt -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "short_inv_DFT_size_4_rescaled" c_dft4_inv_rescaled :: CInt -> CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

foreign import ccall unsafe "short_fwd_DFT_size_8"          c_dft8_fwd          :: CInt -> CInt -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "short_inv_DFT_size_8_unscaled" c_dft8_inv_unscaled :: CInt -> CInt -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "short_inv_DFT_size_8_rescaled" c_dft8_inv_rescaled :: CInt -> CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

foreign import ccall unsafe "short_fwd_DFT_size_16"          c_dft16_fwd          :: CInt -> CInt -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "short_inv_DFT_size_16_unscaled" c_dft16_inv_unscaled :: CInt -> CInt -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "short_inv_DFT_size_16_rescaled" c_dft16_inv_rescaled :: CInt -> CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

----------------------------------------

{-# NOINLINE shortForwardDFT4 #-}
shortForwardDFT4 :: Array Int F -> Array Int F 
shortForwardDFT4 input 
  | arrayLength input /= 4 = error "shortForwardDFT4: expecting an input array of size 4"
  | otherwise              = unsafePerformIO $ do
      allocaArray 4 $ \(ptr1 :: Ptr F) -> do
        allocaArray 4 $ \(ptr2 :: Ptr F) -> do
          pokeArray ptr1 (elems input)
          c_dft4_fwd 1 1 (castPtr ptr1) (castPtr ptr2)
          ys <- peekArray 4 ptr2
          return $ listToArray ys

{-# NOINLINE shortInverseDFT4 #-}
shortInverseDFT4 :: Array Int F -> Array Int F 
shortInverseDFT4 input 
  | arrayLength input /= 4 = error "shortInverseDFT4: expecting an input array of size 4"
  | otherwise              = unsafePerformIO $ do
      allocaArray 4 $ \(ptr1 :: Ptr F) -> do
        allocaArray 4 $ \(ptr2 :: Ptr F) -> do
          pokeArray ptr1 (elems input)
          c_dft4_inv_rescaled 1 1 (castPtr ptr1) (castPtr ptr2)
          ys <- peekArray 4 ptr2
          return $ listToArray ys

----------------------------------------

{-# NOINLINE shortForwardDFT8 #-}
shortForwardDFT8 :: Array Int F -> Array Int F 
shortForwardDFT8 input 
  | arrayLength input /= 8 = error "shortForwardDFT8: expecting an input array of size 8"
  | otherwise              = unsafePerformIO $ do
      allocaArray 8 $ \(ptr1 :: Ptr F) -> do
        allocaArray 8 $ \(ptr2 :: Ptr F) -> do
          pokeArray ptr1 (elems input)
          c_dft8_fwd 1 1 (castPtr ptr1) (castPtr ptr2)
          ys <- peekArray 8 ptr2
          return $ listToArray ys

{-# NOINLINE shortInverseDFT8 #-}
shortInverseDFT8 :: Array Int F -> Array Int F 
shortInverseDFT8 input 
  | arrayLength input /= 8 = error "shortInverseDFT8: expecting an input array of size 8"
  | otherwise              = unsafePerformIO $ do
      allocaArray 8 $ \(ptr1 :: Ptr F) -> do
        allocaArray 8 $ \(ptr2 :: Ptr F) -> do
          pokeArray ptr1 (elems input)
          c_dft8_inv_rescaled 1 1 (castPtr ptr1) (castPtr ptr2)
          ys <- peekArray 8 ptr2
          return $ listToArray ys

----------------------------------------

{-# NOINLINE shortForwardDFT16 #-}
shortForwardDFT16 :: Array Int F -> Array Int F 
shortForwardDFT16 input 
  | arrayLength input /= 16 = error "shortForwardDFT16: expecting an input array of size 16"
  | otherwise              = unsafePerformIO $ do
      allocaArray 16 $ \(ptr1 :: Ptr F) -> do
        allocaArray 16 $ \(ptr2 :: Ptr F) -> do
          pokeArray ptr1 (elems input)
          c_dft16_fwd 1 1 (castPtr ptr1) (castPtr ptr2)
          ys <- peekArray 16 ptr2
          return $ listToArray ys

{-# NOINLINE shortInverseDFT16 #-}
shortInverseDFT16 :: Array Int F -> Array Int F 
shortInverseDFT16 input 
  | arrayLength input /= 16 = error "shortInverseDFT16: expecting an input array of size 16"
  | otherwise              = unsafePerformIO $ do
      allocaArray 16 $ \(ptr1 :: Ptr F) -> do
        allocaArray 16 $ \(ptr2 :: Ptr F) -> do
          pokeArray ptr1 (elems input)
          c_dft16_inv_rescaled 1 1 (castPtr ptr1) (castPtr ptr2)
          ys <- peekArray 16 ptr2
          return $ listToArray ys

--------------------------------------------------------------------------------

testDFT4 :: IO ()
testDFT4 = do
  let sg = getSubgroup (Log2 2)
  xs <- listToArray <$> (replicateM 4 rndIO :: IO [F])
  let ys0 = defForwardDFT sg       xs
  let ys1 = forwardNTT    sg (Poly xs)
  let ys2 = shortForwardDFT4       xs 
  print $ "xs          = " ++ show (elems xs )
  print $ "ys0 (defin) = " ++ show (elems ys0)
  print $ "ys1 (ref)   = " ++ show (elems ys1)
  print $ "ys2 (short) = " ++ show (elems ys2)
  print $ "ok: " ++ show (ys1 == ys2)

testIDFT4 :: IO ()
testIDFT4 = do
  let sg = getSubgroup (Log2 2)
  xs <- listToArray <$> (replicateM 4 rndIO :: IO [F])
  let ys0 =           defInverseDFT sg xs
  let ys1 = fromPoly (inverseNTT    sg xs)
  let ys2 =           shortInverseDFT4 xs 
  print $ "xs          = " ++ show (elems xs )
  print $ "ys0 (defin) = " ++ show (elems ys0)
  print $ "ys1 (ref)   = " ++ show (elems ys1)
  print $ "ys2 (short) = " ++ show (elems ys2)
  print $ "ok: " ++ show (ys1 == ys2)

----------------------------------------

testDFT8 :: IO ()
testDFT8 = do
  let sg = getSubgroup (Log2 3)
  xs <- listToArray <$> (replicateM 8 rndIO :: IO [F])
  let ys0 = defForwardDFT sg       xs
  let ys1 = forwardNTT    sg (Poly xs)
  let ys2 = shortForwardDFT8       xs 
  print $ "xs          = " ++ show (elems xs )
  print $ "ys0 (defin) = " ++ show (elems ys0)
  print $ "ys1 (ref)   = " ++ show (elems ys1)
  print $ "ys2 (short) = " ++ show (elems ys2)
  print $ "ok: " ++ show (ys1 == ys2)

testIDFT8 :: IO ()
testIDFT8 = do
  let sg = getSubgroup (Log2 3)
  xs <- listToArray <$> (replicateM 8 rndIO :: IO [F])
  let ys0 =           defInverseDFT sg xs
  let ys1 = fromPoly (inverseNTT    sg xs)
  let ys2 =           shortInverseDFT8 xs 
  print $ "xs          = " ++ show (elems xs )
  print $ "ys0 (defin) = " ++ show (elems ys0)
  print $ "ys1 (ref)   = " ++ show (elems ys1)
  print $ "ys2 (short) = " ++ show (elems ys2)
  print $ "ok: " ++ show (ys1 == ys2)

--------------------------------------------------------------------------------

-- tmp for debugging
instance Ord F where
  compare a b = compare (fromF a) (fromF b)

testDFT16 :: IO ()
testDFT16 = do
  let sg = getSubgroup (Log2 4)
  xs <- listToArray <$> (replicateM 16 rndIO :: IO [F])
  let ys0 = defForwardDFT sg xs
  let ys1 = forwardNTT    sg (Poly xs)
  let ys2 = experimental_DFT16     xs 
  let ys3 = shortForwardDFT16      xs 
  print $ "xs          = " ++ show (elems xs )
  print $ "ys0 (defin) = " ++ show (elems ys0)
  print $ "ys1 (ref)   = " ++ show (elems ys1)
  print $ "ys2 (hsexp) = " ++ show (elems ys2)
  print $ "ys3 (short) = " ++ show (elems ys3)
  print $ "ok-def: " ++ show (ys0 == ys1)
  print $ "ok-hs:  " ++ show (ys1 == ys2)
  print $ "ok-hs (modulo order):  " ++ show (sort (elems ys1) == sort (elems ys2))
  print $ "ok-C:   " ++ show (ys1 == ys3)

testIDFT16 :: IO ()
testIDFT16 = do
  let sg = getSubgroup (Log2 4)
  xs <- listToArray <$> (replicateM 16 rndIO :: IO [F])
  let ys0 =           defInverseDFT sg xs
  let ys1 = fromPoly (inverseNTT    sg xs)
  let ys2 =     experimental_IDFT16    xs 
  let ys3 =       shortInverseDFT16    xs 
  print $ "xs          = " ++ show (elems xs )
  print $ "ys0 (defin) = " ++ show (elems ys0)
  print $ "ys1 (ref)   = " ++ show (elems ys1)
  print $ "ys2 (hsexp) = " ++ show (elems ys2)
  print $ "ys3 (short) = " ++ show (elems ys3)
  print $ "ok-def: " ++ show (ys0 == ys1)
  print $ "ok-hs:  " ++ show (ys1 == ys2)
  print $ "ok-C:   " ++ show (ys1 == ys3)

--------------------------------------------------------------------------------

dft2_omega, dft4_omega, dft8_omega, dft16_omega :: F
dft2_omega  = subgroupGen $ getSubgroup (Log2 1)
dft4_omega  = subgroupGen $ getSubgroup (Log2 2)
dft8_omega  = subgroupGen $ getSubgroup (Log2 3)
dft16_omega = subgroupGen $ getSubgroup (Log2 4)

dft2_inv_omega  = inverse  dft2_omega  
dft4_inv_omega  = inverse  dft4_omega  
dft8_inv_omega  = inverse  dft8_omega  
dft16_inv_omega = inverse dft16_omega 

--------------------------------------------------------------------------------
-- * Size = 4

dft4_j      = dft4_omega
idft4_inv_4 = 1 / (4 :: F)

printIDFT4 = do
  putStrLn $ "const uint64_t IDFT4_OMEGA         = " ++ show dft4_omega         ++ " ;"  
  putStrLn $ "const uint64_t IDFT4_INV_OMEGA     = " ++ show dft4_inv_omega     ++ " ;"
  putStrLn $ "const uint64_t IDFT4_J             = " ++ show dft4_j             ++ " ;"
  putStrLn $ "const uint64_t IDFT4_INV_4         = " ++ show idft4_inv_4         ++ " ;"

--------------------------------------------------------------------------------
-- * Size = 8

dft8_j             = square dft8_omega
dft8_cos_u         = - (dft8_omega + dft8_omega^7) / 2
dft8_j_sin_u       = - (dft8_omega - dft8_omega^7) / 2
dft8_minus_j_sin_u = - dft8_j_sin_u

printDFT8 = do
  putStrLn $ "const uint64_t DFT8_OMEGA         = " ++ show dft8_omega         ++ " ;"  
  putStrLn $ "const uint64_t DFT8_INV_OMEGA     = " ++ show dft8_inv_omega     ++ " ;"
  putStrLn $ "const uint64_t DFT8_J             = " ++ show dft8_j             ++ " ;"
  putStrLn $ "const uint64_t DFT8_COS_U         = " ++ show dft8_cos_u         ++ " ;"
  putStrLn $ "const uint64_t DFT8_MINUS_J_SIN_U = " ++ show dft8_minus_j_sin_u ++ " ;"

idft8_j             = square dft8_omega
idft8_cos_u         = (dft8_omega + dft8_omega^7) / 2
idft8_j_sin_u       = (dft8_omega - dft8_omega^7) / 2
idft8_minus_j_sin_u = - idft8_j_sin_u
idft8_inv_8         = 1 / (8 :: F)

printIDFT8 = do
  putStrLn $ "const uint64_t IDFT8_OMEGA         = " ++ show  dft8_omega         ++ " ;"  
  putStrLn $ "const uint64_t IDFT8_INV_OMEGA     = " ++ show  dft8_inv_omega     ++ " ;"
  putStrLn $ "const uint64_t IDFT8_J             = " ++ show idft8_j             ++ " ;"
  putStrLn $ "const uint64_t IDFT8_COS_U         = " ++ show idft8_cos_u         ++ " ;"
  putStrLn $ "const uint64_t IDFT8_MINUS_J_SIN_U = " ++ show idft8_minus_j_sin_u ++ " ;"
  putStrLn $ "const uint64_t IDFT8_INV_8         = " ++ show idft8_inv_8         ++ " ;"

--------------------------------------------------------------------------------
-- * Size = 16

dft16_j             = square (square dft16_omega)

dft16_cos_u         = (dft16_omega   + dft16_omega^15) / 2
dft16_cos_2u        = (dft16_omega^2 + dft16_omega^14) / 2
dft16_cos_3u        = (dft16_omega^3 + dft16_omega^13) / 2

dft16_j_sin_u       = (dft16_omega   - dft16_omega^15) / 2      -- ????
dft16_j_sin_2u      = (dft16_omega^2 - dft16_omega^14) / 2      -- but it seems to work...
dft16_j_sin_3u      = (dft16_omega^3 - dft16_omega^13) / 2

printDFT16 :: IO ()
printDFT16 = do
  putStrLn $ "const uint64_t DFT16_OMEGA          = " ++ show dft16_omega         ++ " ;"  
  putStrLn $ "const uint64_t DFT16_INV_OMEGA      = " ++ show dft16_inv_omega     ++ " ;"
  putStrLn $ "const uint64_t DFT16_J              = " ++ show dft16_j             ++ " ;"
  putStrLn $ "const uint64_t DFT16_COS_U          = " ++ show dft16_cos_u         ++ " ;"
  putStrLn $ "const uint64_t DFT16_COS_2U         = " ++ show dft16_cos_2u        ++ " ;"
  putStrLn $ "const uint64_t DFT16_COS_3U         = " ++ show dft16_cos_3u        ++ " ;"
  putStrLn $ "const uint64_t DFT16_MINUS_J_SIN_U  = " ++ show (- dft16_j_sin_u )  ++ " ;"
  putStrLn $ "const uint64_t DFT16_MINUS_J_SIN_2U = " ++ show (- dft16_j_sin_2u)  ++ " ;"
  putStrLn $ "const uint64_t DFT16_MINUS_J_SIN_3U = " ++ show (- dft16_j_sin_3u)  ++ " ;"

  putStrLn $ "const uint64_t DFT16_COS_3U_PLUS_U          = " ++ show (  dft16_cos_3u   + dft16_cos_u  )  ++ " ;"
  putStrLn $ "const uint64_t DFT16_COS_3U_MINUS_U         = " ++ show (  dft16_cos_3u   - dft16_cos_u  )  ++ " ;"
  putStrLn $ "const uint64_t DFT16_J_SIN_3U_MINUS_U       = " ++ show (  dft16_j_sin_3u - dft16_j_sin_u)  ++ " ;"
  putStrLn $ "const uint64_t DFT16_J_SIN_MINUS_3U_MINUS_U = " ++ show (- dft16_j_sin_3u - dft16_j_sin_u)  ++ " ;"

--------------------

idft16_inv_16        = 1 / (16 :: F)
idft16_j             = square (square dft16_omega)

idft16_cos_u         = (dft16_omega   + dft16_omega^15) / 2
idft16_cos_2u        = (dft16_omega^2 + dft16_omega^14) / 2
idft16_cos_3u        = (dft16_omega^3 + dft16_omega^13) / 2

idft16_j_sin_u       = (dft16_omega   - dft16_omega^15) / 2
idft16_j_sin_2u      = (dft16_omega^2 - dft16_omega^14) / 2
idft16_j_sin_3u      = (dft16_omega^3 - dft16_omega^13) / 2

printIDFT16 :: IO ()
printIDFT16 = do
  putStrLn $ "const uint64_t IDFT16_OMEGA          = " ++ show  dft16_omega         ++ " ;"  
  putStrLn $ "const uint64_t IDFT16_INV_OMEGA      = " ++ show  dft16_inv_omega     ++ " ;"
  putStrLn $ "const uint64_t IDFT16_INV_16         = " ++ show idft16_inv_16        ++ " ;"
  putStrLn $ "const uint64_t IDFT16_J              = " ++ show idft16_j             ++ " ;"
  putStrLn $ "const uint64_t IDFT16_COS_U          = " ++ show idft16_cos_u         ++ " ;"
  putStrLn $ "const uint64_t IDFT16_COS_2U         = " ++ show idft16_cos_2u        ++ " ;"
  putStrLn $ "const uint64_t IDFT16_COS_3U         = " ++ show idft16_cos_3u        ++ " ;"
  putStrLn $ "const uint64_t IDFT16_MINUS_J_SIN_U  = " ++ show (- idft16_j_sin_u )  ++ " ;"
  putStrLn $ "const uint64_t IDFT16_MINUS_J_SIN_2U = " ++ show (- idft16_j_sin_2u)  ++ " ;"
  putStrLn $ "const uint64_t IDFT16_MINUS_J_SIN_3U = " ++ show (- idft16_j_sin_3u)  ++ " ;"

  putStrLn $ "const uint64_t IDFT16_COS_3U_PLUS_U          = " ++ show (  idft16_cos_3u   + idft16_cos_u  )  ++ " ;"
  putStrLn $ "const uint64_t IDFT16_COS_3U_MINUS_U         = " ++ show (  idft16_cos_3u   - idft16_cos_u  )  ++ " ;"
  putStrLn $ "const uint64_t IDFT16_J_SIN_3U_MINUS_U       = " ++ show (  idft16_j_sin_3u - idft16_j_sin_u)  ++ " ;"
  putStrLn $ "const uint64_t IDFT16_J_SIN_MINUS_3U_MINUS_U = " ++ show (- idft16_j_sin_3u - idft16_j_sin_u)  ++ " ;"

--------------------------------------------------------------------------------

experimental_IDFT16 :: Array Int F -> Array Int F 
experimental_IDFT16 input = output where

  rescale :: F -> F
  rescale x = x / 16

  [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15] = elems input
  output = listArray (0,15) $ map rescale [y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15]

  t1  = x0  + x8 
  t2  = x4  + x12
  t3  = x2  + x10
  t4  = x2  - x10
  t5  = x6  + x14
  t6  = x6  - x14
  t7  = x1  + x9 
  t8  = x1  - x9 
  t9  = x3  + x11
  t10 = x3  - x11
  t11 = x5  + x13
  t12 = x5  - x13
  t13 = x7  + x15
  t14 = x7  - x15
  t15 = t1  + t2 
  t16 = t3  + t5 
  t17 = t15 + t16
  t18 = t7  + t11
  t19 = t7  - t11
  t20 = t9  + t13
  t21 = t9  - t13
  t22 = t18 + t20
  t23 = t8  + t14
  t24 = t8  - t14
  t25 = t10 + t12
  t26 = t12 - t10   

  m0  = t17 + t22
  m1  = t17 - t22
  m2  = t15 - t16
  m3  = t1  - t2
  m4  = x0  - x8

  m5  = idft16_cos_2u * (t19 - t21)
  m6  = idft16_cos_2u * (t4  - t6 )
  m7  = idft16_cos_3u * (t24 + t26)
  m8  = (idft16_cos_3u + idft16_cos_u) * t24
  m9  = (idft16_cos_3u - idft16_cos_u) * t26
  m10 = idft16_j * (t20 - t18)
  m11 = idft16_j * (t5  - t3 )
  m12 = idft16_j * (x12 - x4 )
  m13 = - idft16_j_sin_2u * ( t19 + t21)
  m14 = - idft16_j_sin_2u * ( t4  + t6 )
  m15 = - idft16_j_sin_3u * ( t23 + t25)
  m16 =   (idft16_j_sin_3u - idft16_j_sin_u) * t23
  m17 = - (idft16_j_sin_3u + idft16_j_sin_u) * t25
  
  s7  = m8  - m7
  s8  = m9  - m7

  s15 = m15 + m16
  s16 = m15 - m17

  s1  = m3  + m5
  s2  = m3  - m5
  s3  = m11 + m13
  s4  = m13 - m11
  s5  = m4  + m6
  s6  = m4  - m6

  s9  = s5  + s7
  s10 = s5  - s7
  s11 = s6  + s8
  s12 = s6  - s8

  s13 = m12 + m14
  s14 = m12 - m14

  s17 = s13 + s15
  s18 = s13 - s15
  s19 = s14 + s16
  s20 = s14 - s16

  y0  = m0
  y1  = s9  + s17
  y2  = s1  + s3
  y3  = s12 - s20
  y4  = m2  + m10
  y5  = s11 + s19
  y6  = s2  + s4
  y7  = s10 - s18
  y8  = m1
  y9  = s10 + s18
  y10 = s2  - s4
  y11 = s11 - s19
  y12 = m2  - m10
  y13 = s12 + s20
  y14 = s1  - s3
  y15 = s9  - s17

----------------------------------------

-- is it always true, that in the NTT setting, the DFT and the unscaled 
-- inverse DFT are the same up to permutation?? 
--
experimental_DFT16 :: Array Int F -> Array Int F 
experimental_DFT16 input = output where

  [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15] = elems input
  output = listArray (0,15) [y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15]

  t1  = x0  + x8 
  t2  = x4  + x12
  t3  = x2  + x10
  t4  = x2  - x10
  t5  = x6  + x14
  t6  = x6  - x14
  t7  = x1  + x9 
  t8  = x1  - x9 
  t9  = x3  + x11
  t10 = x3  - x11
  t11 = x5  + x13
  t12 = x5  - x13
  t13 = x7  + x15
  t14 = x7  - x15
  t15 = t1  + t2 
  t16 = t3  + t5 
  t17 = t15 + t16
  t18 = t7  + t11
  t19 = t7  - t11
  t20 = t9  + t13
  t21 = t9  - t13
  t22 = t18 + t20
  t23 = t8  + t14
  t24 = t8  - t14
  t25 = t10 + t12
  t26 = t12 - t10   

  m0  = t17 + t22
  m1  = t17 - t22
  m2  = t15 - t16
  m3  = t1  - t2
  m4  = x0  - x8

  m5  = dft16_cos_2u * (t19 - t21)
  m6  = dft16_cos_2u * (t4  - t6 )
  m7  = dft16_cos_3u * (t24 + t26)
  m8  = (dft16_cos_3u + dft16_cos_u) * t24
  m9  = (dft16_cos_3u - dft16_cos_u) * t26
  m10 = dft16_j * (t20 - t18)
  m11 = dft16_j * (t5  - t3 )
  m12 = dft16_j * (x12 - x4 )
  m13 = - dft16_j_sin_2u * ( t19 + t21)
  m14 = - dft16_j_sin_2u * ( t4  + t6 )
  m15 = - dft16_j_sin_3u * ( t23 + t25)
  m16 =   (dft16_j_sin_3u - dft16_j_sin_u) * t23
  m17 = - (dft16_j_sin_3u + dft16_j_sin_u) * t25
  
  s7  = m8  - m7
  s8  = m9  - m7

  s15 = m15 + m16
  s16 = m15 - m17

  s1  = m3  + m5
  s2  = m3  - m5
  s3  = m11 + m13
  s4  = m13 - m11
  s5  = m4  + m6
  s6  = m4  - m6

  s9  = s5  + s7
  s10 = s5  - s7
  s11 = s6  + s8
  s12 = s6  - s8

  s13 = m12 + m14
  s14 = m12 - m14

  s17 = s13 + s15
  s18 = s13 - s15
  s19 = s14 + s16
  s20 = s14 - s16

  y0  = m0       
  y1  = s9  - s17
  y2  = s1  - s3 
  y3  = s12 + s20
  y4  = m2  - m10
  y5  = s11 - s19
  y6  = s2  - s4 
  y7  = s10 + s18
  y8  = m1       
  y9  = s10 - s18
  y10 = s2  + s4 
  y11 = s11 + s19
  y12 = m2  + m10
  y13 = s12 - s20
  y14 = s1  + s3 
  y15 = s9  + s17

--------------------------------------------------------------------------------
  
data Cost = MkCost
  { _nAdds :: Int
  , _nMuls :: Int
  }
  deriving (Eq,Show)

addCost :: Cost -> Cost -> Cost
addCost (MkCost a1 m1) (MkCost a2 m2) = MkCost (a1+a2) (m1+m2)

scaleCost :: Int -> Cost -> Cost
scaleCost s (MkCost a m) = MkCost (s*a) (s*m)

doubleCost :: Cost -> Cost
doubleCost = scaleCost 2

estimateNTTCost :: Log2 -> Cost
estimateNTTCost = go where
  go :: Log2 -> Cost
  go 0 = MkCost 0 0
  go 1 = MkCost 2 0
  go m = recursive `addCost` post where
    recursive   = doubleCost (go (m-1))
    post        = scaleCost halfN (MkCost 3 1)
    halfN       = exp2_ (m-1)

--------------------------------------------------------------------------------

