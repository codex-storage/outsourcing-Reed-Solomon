
module NTT.Class where

--------------------------------------------------------------------------------

import Data.Kind

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
