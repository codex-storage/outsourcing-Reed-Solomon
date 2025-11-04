
{-# LANGUAGE TypeFamilies #-}
module Class.Vector where

--------------------------------------------------------------------------------

import Data.Kind

import Data.Array.IArray

import Class.Field
import Misc

--------------------------------------------------------------------------------

class Vector v where

  type VecElem v :: Type

  vecLength :: v -> Int
  vecPeek   :: v -> Int -> VecElem v
  
  vecToList    :: v -> [VecElem v]
  vecFromList  :: [VecElem v] -> v

  vecToArray   :: v -> Array Int (VecElem v)
  vecFromArray :: Array Int (VecElem v) -> v

  vecTake   :: Int -> v -> v
  vecDrop   :: Int -> v -> v
  vecAppend :: v -> v -> v
  vecConcat :: [v] -> v
  vecMap    :: (VecElem v -> VecElem v) -> v -> v

vecSize :: Vector v => v -> Int
vecSize = vecLength

--------------------------------------------------------------------------------

instance Vector [a] where
  type VecElem [a] = a
  vecLength    = length
  vecPeek      = (!!)
  vecToList    = id
  vecFromList  = id
  vecToArray   = listToArray
  vecFromArray = elems
  vecTake      = take
  vecDrop      = drop
  vecAppend    = (++)
  vecConcat    = concat
  vecMap       = map

instance Vector (Array Int a) where
  type VecElem (Array Int a) = a
  vecLength    = \arr -> let (0,n1) = bounds arr in n1+1
  vecPeek      = (!)
  vecToList    = elems
  vecFromList  = listToArray
  vecToArray   = id
  vecFromArray = id
  vecTake      = takeArray
  vecDrop      = dropArray
  vecAppend    = appendArrays
  vecConcat    = concatArrays
  vecMap       = amap

--------------------------------------------------------------------------------
-- * Pointwise operations

class PointwiseGroup a where
  -- | Pointwise negation
  pwNeg :: a -> a 
  -- | Pointwise addition
  pwAdd :: a -> a -> a
  -- | Pointwise subtraction
  pwSub :: a -> a -> a

class PointwiseGroup a => PointwiseRing a where
  -- | Pointwise squaring
  pwSqr :: a -> a
  -- | Pointwise multiplication
  pwMul :: a -> a -> a
  -- | Pointwise @a*b+c@
  pwMulAdd :: a -> a -> a -> a
  -- | Pointwise @a*b-c@
  pwMulSub :: a -> a -> a -> a

class PointwiseRing a => PointwiseField a where
  -- | Pointwise inversion
  pwInv :: a -> a
  -- | Pointwise division
  pwDiv :: a -> a -> a

--------------------------------------------------------------------------------

-- | Finite dimensional vector spaces
class (Vector v, Field (VecElem v), PointwiseField v) => VectorSpace v where
  -- | Scaling by an element
  vecScale :: VecElem v -> v -> v
  -- | Dot product
  dotProd :: v -> v -> VecElem v
  -- | The array @[ a*b^k | k<-[0..n-1] ]@
  powers :: VecElem v -> VecElem v -> Int -> v
  -- | Pointwise multiplication by the array @[ a*b^k | k<-[0..n-1] ]@
  mulByPowers :: VecElem v -> VecElem v -> v -> v
  -- | Linear combination @a*x + y@
  linComb1 :: (VecElem v, v) -> v -> v
  -- | Linear combination @a*x + b*y@
  linComb2 :: (VecElem v, v) -> (VecElem v, v) -> v

--------------------------------------------------------------------------------
