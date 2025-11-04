
{-# LANGUAGE TypeFamilies #-}
module Field.Class where

--------------------------------------------------------------------------------

import Data.Kind
import Data.Proxy

import System.Random

import qualified Field.Goldilocks           as Goldi   
import qualified Field.Goldilocks.Extension as GoldiExt

--------------------------------------------------------------------------------

class (Show a, Eq a, Num a, Fractional a) => Field a where
  fieldSize :: Proxy a -> Integer
  zero      :: a
  one       :: a
  isZero    :: a -> Bool
  isOne     :: a -> Bool
  square    :: a -> a
  power     :: a -> Integer -> a
  power_    :: a -> Int     -> a
  rndIO     :: IO a

inverse :: Field a => a -> a
inverse = recip

-- | Quadratic extensions
class (Field (BaseField ext), Field ext) => QuadraticExt ext where
  type BaseField ext :: Type
  inject  :: BaseField ext -> ext
  project :: ext -> Maybe (BaseField ext)
  scale   :: BaseField ext -> ext -> ext
  quadraticPack   :: (BaseField ext, BaseField ext) -> ext
  quadraticUnpack :: ext -> (BaseField ext, BaseField ext)

--------------------------------------------------------------------------------

instance Field Goldi.F where
  fieldSize _ = Goldi.goldilocksPrime
  zero        = Goldi.zero
  one         = Goldi.one
  isZero      = Goldi.isZero
  isOne       = Goldi.isOne
  square      = Goldi.sqr
  power       = Goldi.pow
  power_      = Goldi.pow_
  rndIO       = randomIO

--------------------------------------------------------------------------------

instance Field GoldiExt.FExt where
  fieldSize _ = (Goldi.goldilocksPrime ^ 2)
  zero        = GoldiExt.zero
  one         = GoldiExt.one
  isZero      = GoldiExt.isZero
  isOne       = GoldiExt.isOne
  square      = GoldiExt.sqr
  power       = GoldiExt.pow
  power_      = GoldiExt.pow_
  rndIO       = randomIO

--------------------------------------------------------------------------------

instance QuadraticExt GoldiExt.FExt where
  type BaseField GoldiExt.FExt = Goldi.F

  inject  = GoldiExt.inj
  project = GoldiExt.proj
  scale   = GoldiExt.scl
  quadraticPack   = GoldiExt.pack
  quadraticUnpack = GoldiExt.unpack

--------------------------------------------------------------------------------
