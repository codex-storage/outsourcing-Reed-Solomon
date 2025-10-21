
-- | Reference (slow) implementation of the Goldilocks prime field

{-# LANGUAGE BangPatterns, NumericUnderscores #-}
module Field.Goldilocks.Slow where

--------------------------------------------------------------------------------

import Prelude hiding ( div )
import qualified Prelude

import Data.Bits
import Data.Word
import Data.Ratio

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal

import System.Random

import Data.Binary
import Data.Binary.Get ( getWord64le )
import Data.Binary.Put ( putWord64le )

import Text.Printf

import Data.Flat

--------------------------------------------------------------------------------

type F = Goldilocks

fromF :: F -> Word64
fromF (MkGoldilocks x) = fromInteger x

toF :: Word64 -> F
toF = mkGoldilocks . fromIntegral

intToF :: Int -> F
intToF = mkGoldilocks . fromIntegral

--------------------------------------------------------------------------------

instance Binary F where
  put x = putWord64le (fromF x)
  get = toF <$> getWord64le

instance Storable F where
  peek ptr                   = (MkGoldilocks . fromIntegral) <$> peek (castPtr ptr :: Ptr Word64)
  poke ptr  (MkGoldilocks x) = poke (castPtr ptr :: Ptr Word64) (fromInteger x :: Word64)
  sizeOf    _ = 8
  alignment _ = 8

instance Flat Goldilocks where
  sizeInBytes  _ = 8
  sizeInQWords _ = 1
  withFlat (MkGoldilocks x) action = alloca $ \ptr -> poke ptr (fromInteger x :: Word64) >> action ptr
  makeFlat ptr = (MkGoldilocks . fromIntegral) <$> peek ptr

--------------------------------------------------------------------------------

newtype Goldilocks 
  = MkGoldilocks Integer 
  deriving Eq

instance Show Goldilocks where
  show (MkGoldilocks k) = printf "0x%016x" k

zero, one, two :: Goldilocks
zero = MkGoldilocks 0
one  = MkGoldilocks 1
two  = MkGoldilocks 2

isZero, isOne :: Goldilocks -> Bool
isZero (MkGoldilocks x) = x == 0
isOne  (MkGoldilocks x) = x == 1

--------------------------------------------------------------------------------

instance Num Goldilocks where
  fromInteger = mkGoldilocks
  negate = neg
  (+)    = add
  (-)    = sub
  (*)    = mul
  abs    = id
  signum _ = MkGoldilocks 1

instance Fractional Goldilocks where
  fromRational y = fromInteger (numerator y) `div` fromInteger (denominator y)
  recip  = inv
  (/)    = div

instance Random Goldilocks where
  -- random :: RandomGen g => g -> (a, g) 
  random  g = let (x,g') = randomR (0,goldilocksPrime-1) g in (MkGoldilocks x, g')
  randomR = error "randomR/Goldilocks: doesn't make much sense"

--------------------------------------------------------------------------------

-- | @p = 2^64 - 2^32 + 1@
goldilocksPrime :: Integer
goldilocksPrime = 0x_ffff_ffff_0000_0001

modp :: Integer -> Integer
modp a = mod a goldilocksPrime

mkGoldilocks :: Integer -> Goldilocks
mkGoldilocks = MkGoldilocks . modp

-- | A fixed generator of the multiplicative subgroup of the field
theMultiplicativeGenerator :: Goldilocks
theMultiplicativeGenerator = mkGoldilocks 7

--------------------------------------------------------------------------------

neg :: Goldilocks -> Goldilocks
neg (MkGoldilocks k) = mkGoldilocks (negate k) 

add :: Goldilocks -> Goldilocks -> Goldilocks
add (MkGoldilocks a) (MkGoldilocks b) = mkGoldilocks (a+b) 

sub :: Goldilocks -> Goldilocks -> Goldilocks
sub (MkGoldilocks a) (MkGoldilocks b) = mkGoldilocks (a-b) 

sqr :: Goldilocks -> Goldilocks
sqr x = mul x x

mul :: Goldilocks -> Goldilocks -> Goldilocks
mul (MkGoldilocks a) (MkGoldilocks b) = mkGoldilocks (a*b) 

inv :: Goldilocks -> Goldilocks
inv x = pow x (goldilocksPrime - 2)

div :: Goldilocks -> Goldilocks -> Goldilocks
div a b = mul a (inv b)

--------------------------------------------------------------------------------

pow_ :: Goldilocks -> Int -> Goldilocks
pow_ x e = pow x (fromIntegral e)

pow :: Goldilocks -> Integer -> Goldilocks
pow x e 
  | e == 0    = 1
  | e <  0    = pow (inv x) (negate e)
  | otherwise = go 1 x e
  where
    go !acc _  0     = acc
    go !acc !s !expo = case expo .&. 1 of
      0 -> go acc     (sqr s) (shiftR expo 1)
      _ -> go (acc*s) (sqr s) (shiftR expo 1)

--------------------------------------------------------------------------------

