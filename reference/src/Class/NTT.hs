
module NTT.Class where

--------------------------------------------------------------------------------

import Data.Kind

import Class.Field
import Class.Poly

import NTT.Subgroup

--------------------------------------------------------------------------------

{-
class NTT ntt where
  type Field ntt :: Type
  type Poly  ntt :: Type

  forwardNTT        :: Subgroup F -> Poly F -> FlatArray F
  inverseNTT        :: Subgroup F -> FlatArray F -> Poly F
  shiftedForwardNTT :: Subgroup F -> F -> Poly F -> FlatArray F
  shiftedInverseNTT :: Subgroup F -> F -> FlatArray F -> Poly F
  asymmForwardNTT   :: Subgroup F -> Poly F -> Subgroup F -> FlatArray F
-}

{-
-- | Polynomials which over an FFT-friend field support NTT operations
class (Univariate p, FFTField (Coeff p)) => UnivariateFFT p where
  -- | Number-theoretical transform (evaluate on a subgroup)
  ntt  :: FFTSubgroup (Coeff p) -> p -> FlatArray (Coeff p) 
  -- | Inverse number-theoretical transform (interpolate on a subgroup)
  intt :: FFTSubgroup (Coeff p) -> FlatArray (Coeff p) -> p
  -- | Shifts @f@ by @eta@, evaluating @f(eta*omega^k)@
  shiftedNTT  :: FFTSubgroup (Coeff p) -> Coeff p -> p -> FlatArray (Coeff p) 
  -- | Shifts @f@ by @eta^-1@, interpolating @f@ so that @f(eta^-1 * omega^k) = y_k@
  shiftedINTT :: FFTSubgroup (Coeff p) -> Coeff p -> FlatArray (Coeff p) -> p
  -- | Evaluate on a larger subgroup than the polynomial is defined on
  asymmNTT :: FFTSubgroup (Coeff p) -> p -> FFTSubgroup (Coeff p) -> FlatArray (Coeff p) 
-}
