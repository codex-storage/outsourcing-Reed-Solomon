
{-# LANGUAGE ScopedTypeVariables #-}
module NTT.FFT.Slow where

--------------------------------------------------------------------------------

import Data.Array
import Data.Bits

import NTT.Poly.Naive
import NTT.Subgroup

import Field.Goldilocks
import Field.Goldilocks.Extension ( FExt , scl , inj )

import Misc

--------------------------------------------------------------------------------

-- | Evaluate the polynomial on a multiplicative subgroup /of the same size/, using FFT 
polyEvaluate :: Subgroup F -> Poly F -> Array Int F 
polyEvaluate = subgroupNTT

-- | Interpolate the values on a multiplicative subgroup into a polynomial, using inverse FFT 
polyInterpolate :: Subgroup F -> Array Int F -> Poly F 
polyInterpolate = subgroupINTT

--------------------------------------------------------------------------------
-- compatible names

forwardNTT :: Subgroup F -> Poly F -> Array Int F
forwardNTT = subgroupNTT

inverseNTT :: Subgroup F -> Array Int F -> Poly F
inverseNTT = subgroupINTT

--------------------------------------------------------------------------------

-- | Evaluates a polynomial on a subgroup /of the same size/
subgroupNTT :: Subgroup F -> Poly F -> Array Int F
subgroupNTT subgroup (Poly coeffs)
  | n1+1 /= subgroupOrder subgroup = error "ntt: input size does not match the subgroup order"
  | n1 == 0   = listArray (0,0) [coeffs!0]
  | otherwise = final
  where
    (0,n1) = bounds coeffs
    n      = n1 + 1
    hn     = Prelude.div n 2
    hn1    = hn - 1
    hsub   = halveSubgroup subgroup
    g      = subgroupGen   subgroup
    v_even = elems $ subgroupNTT hsub $ Poly $ listArray (0,hn1) [ coeffs!(2*i  ) | i<-[0..hn1] ]
    v_odd  = elems $ subgroupNTT hsub $ Poly $ listArray (0,hn1) [ coeffs!(2*i+1) | i<-[0..hn1] ]
    gpows  = powersOf hn g
    first  = zipWith3 (\gk x y -> (x + gk * y)) gpows v_even v_odd
    second = zipWith3 (\gk x y -> (x - gk * y)) gpows v_even v_odd
    final  = listArray (0,n1) (first ++ second) 

-- | Evaluates a polynomial on a subgroup /of the same size/
subgroupNTTExt :: Subgroup F -> Poly FExt -> Array Int FExt
subgroupNTTExt subgroup (Poly coeffs)
  | n1+1 /= subgroupOrder subgroup = error "ntt: input size does not match the subgroup order"
  | n1 == 0   = listArray (0,0) [coeffs!0]
  | otherwise = final
  where
    (0,n1) = bounds coeffs
    n      = n1 + 1
    hn     = Prelude.div n 2
    hn1    = hn - 1
    hsub   = halveSubgroup subgroup
    g      = subgroupGen   subgroup
    v_even = elems $ subgroupNTTExt hsub $ Poly $ listArray (0,hn1) [ coeffs!(2*i  ) | i<-[0..hn1] ]
    v_odd  = elems $ subgroupNTTExt hsub $ Poly $ listArray (0,hn1) [ coeffs!(2*i+1) | i<-[0..hn1] ]
    gpows  = powersOf hn g
    first  = zipWith3 (\gk x y -> (x + gk `scl` y)) gpows v_even v_odd
    second = zipWith3 (\gk x y -> (x - gk `scl` y)) gpows v_even v_odd
    final  = listArray (0,n1) (first ++ second) 

----------------------------------------

-- | Interpolates values into a polynomial on a subgroup
subgroupINTT :: Subgroup F -> Array Int F -> Poly F
subgroupINTT subgroup values 
  | n1+1 /= subgroupOrder subgroup = error "intt: input size does not match the subgroup order"
  | n1 == 0   = Poly $ listArray (0,0) [values!0]
  | otherwise = final
  where
    (0,n1) = bounds values
    n      = n1 + 1
    hn     = Prelude.div n 2
    hn1    = hn - 1
    hsub   = halveSubgroup subgroup
    g      = subgroupGen   subgroup
    first  = [ values!(i   ) | i<-[0..hn1] ]
    second = [ values!(i+hn) | i<-[0..hn1] ]
    gpows  = powersOf hn g
    v_even = zipWith  (\  x y -> (x + y) /  2   )       first second
    v_odd  = zipWith3 (\g x y -> (x - y) / (2*g)) gpows first second
    p_even = elems $ polyCoeffArray $ subgroupINTT hsub $ listArray (0,hn1) v_even
    p_odd  = elems $ polyCoeffArray $ subgroupINTT hsub $ listArray (0,hn1) v_odd
    final  = Poly $ listArray (0,n1) (interleave p_even p_odd) 

-- | Interpolates values into a polynomial on a subgroup
subgroupINTTExt :: Subgroup F -> Array Int FExt -> Poly FExt
subgroupINTTExt subgroup values 
  | n1+1 /= subgroupOrder subgroup = error "intt: input size does not match the subgroup order"
  | n1 == 0   = Poly $ listArray (0,0) [values!0]
  | otherwise = final
  where
    (0,n1) = bounds values
    n      = n1 + 1
    hn     = Prelude.div n 2
    hn1    = hn - 1
    hsub   = halveSubgroup subgroup
    g      = subgroupGen   subgroup
    first  = [ values!(i   ) | i<-[0..hn1] ]
    second = [ values!(i+hn) | i<-[0..hn1] ]
    gpows  = powersOf hn g
    v_even = zipWith  (\  x y -> (x + y) /  2       )       first second
    v_odd  = zipWith3 (\g x y -> (x - y) / (2*inj g)) gpows first second
    p_even = elems $ polyCoeffArray $ subgroupINTTExt hsub $ listArray (0,hn1) v_even
    p_odd  = elems $ polyCoeffArray $ subgroupINTTExt hsub $ listArray (0,hn1) v_odd
    final  = Poly $ listArray (0,n1) (interleave p_even p_odd) 

--------------------------------------------------------------------------------

-- | Evaluates a polynomial on a coset /of the same size/
cosetNTT :: Coset F -> Poly F -> Array Int F
cosetNTT (MkCoset subgroup offset) poly = subgroupNTT subgroup (shiftPolyCoeffs offset poly)

-- | Evaluates a polynomial on a coset /of the same size/
cosetNTTExt :: Coset F -> Poly FExt -> Array Int FExt
cosetNTTExt (MkCoset subgroup offset) poly = subgroupNTTExt subgroup (shiftPolyCoeffsExt offset poly)

-- | Interpolates a polynomial from its values on a coset
cosetINTT :: Coset F -> Array Int F -> Poly F 
cosetINTT (MkCoset subgroup offset) values = shiftPolyCoeffs (recip offset) (subgroupINTT subgroup values)

-- | Interpolates a polynomial from its values on a coset
cosetINTTExt :: Coset F -> Array Int FExt -> Poly FExt 
cosetINTTExt (MkCoset subgroup offset) values = shiftPolyCoeffsExt (recip offset) (subgroupINTTExt subgroup values)

----------------------------------------

-- | multiplies the @k@-th coefficient with @eta^k@
shiftPolyCoeffs :: F -> Poly F -> Poly F
shiftPolyCoeffs eta (Poly coeffs) = Poly (arrayPointWiseProduct (powersOf n eta) coeffs) where
  n = arrayLength coeffs

-- | multiplies the @k@-th coefficient with @eta^k@
shiftPolyCoeffsExt :: F -> Poly FExt -> Poly FExt
shiftPolyCoeffsExt eta (Poly coeffs) = Poly (arrayPointWiseScale (powersOf n eta) coeffs) where
  n = arrayLength coeffs

-- | pointwise product of a list and array
arrayPointWiseProduct :: [F] -> Array Int F ->  Array Int F
arrayPointWiseProduct list array 
  = listArray (bounds array) 
  $ safeZipWith (*) list (elems array)

-- | pointwise product of a list and array
arrayPointWiseScale :: [F] -> Array Int FExt ->  Array Int FExt
arrayPointWiseScale list array 
  = listArray (bounds array) 
  $ safeZipWith scl list (elems array)

--------------------------------------------------------------------------------

-- | Evaluates a polynomial on a coset larger than the polynomial
asymmetricCosetNTT :: Coset F -> Poly F -> Array Int F
asymmetricCosetNTT coset@(MkCoset subgroup shift) poly
  | r /= 0     = error "asymmetricCosetNTT: we expect a subgroup whose size is a multiple of the polynomial's size"
  | otherwise  = interleaveArrays pieces
  where
    n = subgroupOrder subgroup
    g = subgroupGen   subgroup
    m = polySize poly
    (q,r) = divMod n m
    smallSubgroup = powSubgroup subgroup q
    pieces = [ cosetNTT (MkCoset smallSubgroup (shift * g^j)) poly | j <- [0..q-1] ]

-- | Evaluates a polynomial on a coset larger than the polynomial
asymmetricCosetNTTExt :: Coset F -> Poly FExt -> Array Int FExt
asymmetricCosetNTTExt coset@(MkCoset subgroup shift) poly
  | r /= 0     = error "asymmetricCosetNTTExt: we expect a subgroup whose size is a multiple of the polynomial's size"
  | otherwise  = interleaveArrays pieces
  where
    n = subgroupOrder subgroup
    g = subgroupGen   subgroup
    m = polySize poly
    (q,r) = divMod n m
    smallSubgroup = powSubgroup subgroup q
    pieces = [ cosetNTTExt (MkCoset smallSubgroup (shift * g^j)) poly | j <- [0..q-1] ]

--------------------------------------------------------------------------------
