
{-# LANGUAGE TypeFamilies #-}
module Class.Field where

--------------------------------------------------------------------------------

import Data.Kind
import Data.Proxy

import System.Random

--------------------------------------------------------------------------------

class (Show a, Eq a, Num a) => Ring a where
  zero      :: a
  one       :: a
  isZero    :: a -> Bool
  isOne     :: a -> Bool
  square    :: a -> a
  power     :: a -> Integer -> a
  power_    :: a -> Int     -> a

class (Ring a, Fractional a) => Field a where
  inverse   :: a -> a
  inverse = recip

class Field a => FiniteField a where
  rndIO     :: IO a
  fieldSize :: Proxy a -> Integer

-- | Quadratic extensions
class (Field (BaseField ext), Field ext) => QuadraticExt ext where
  type BaseField ext :: Type
  inject  :: BaseField ext -> ext
  project :: ext -> Maybe (BaseField ext)
  scale   :: BaseField ext -> ext -> ext
  quadraticPack   :: (BaseField ext, BaseField ext) -> ext
  quadraticUnpack :: ext -> (BaseField ext, BaseField ext)

--------------------------------------------------------------------------------
