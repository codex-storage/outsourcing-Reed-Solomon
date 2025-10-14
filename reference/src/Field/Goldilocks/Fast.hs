
-- | Bindings to a C implementation of the Goldilocks prime field

{-# LANGUAGE ForeignFunctionInterface, BangPatterns, NumericUnderscores #-}
module Field.Goldilocks.Fast where

--------------------------------------------------------------------------------

import Prelude hiding ( div )
import qualified Prelude

import Data.Bits
import Data.Word
import Data.Ratio

import Foreign.C
import Foreign.Ptr
import Foreign.Storable

import System.Random

import Data.Binary
import Data.Binary.Get ( getWord64le )
import Data.Binary.Put ( putWord64le )

import Text.Printf

--------------------------------------------------------------------------------

type F = Goldilocks

fromF :: F -> Word64
fromF (MkGoldilocks x) = x

unsafeToF :: Word64 -> F
unsafeToF = MkGoldilocks

toF :: Word64 -> F
toF = mkGoldilocks . fromIntegral

intToF :: Int -> F
intToF = mkGoldilocks . fromIntegral

instance Binary F where
  put x = putWord64le (fromF x)
  get = toF <$> getWord64le

instance Storable F where
  peek ptr                   = MkGoldilocks <$> peek (castPtr ptr)
  poke ptr  (MkGoldilocks x) = poke (castPtr ptr) x
  sizeOf    _ = 8
  alignment _ = 8

--------------------------------------------------------------------------------

newtype Goldilocks 
  = MkGoldilocks Word64
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
  random  g = let (x,g') = randomR (0,goldilocksPrimeWord64-1) g in (MkGoldilocks x, g')
  randomR = error "randomR/Goldilocks: doesn't make much sense"

--------------------------------------------------------------------------------

-- | @p = 2^64 - 2^32 + 1@
goldilocksPrime :: Integer
goldilocksPrime = 0x_ffff_ffff_0000_0001

goldilocksPrimeWord64 :: Word64
goldilocksPrimeWord64 = 0x_ffff_ffff_0000_0001

modp :: Integer -> Integer
modp a = mod a goldilocksPrime

mkGoldilocks :: Integer -> Goldilocks
mkGoldilocks = MkGoldilocks . fromInteger . modp

-- | A fixed generator of the multiplicative subgroup of the field
theMultiplicativeGenerator :: Goldilocks
theMultiplicativeGenerator = mkGoldilocks 7

--------------------------------------------------------------------------------

foreign import ccall unsafe "goldilocks_neg" c_goldilocks_neg :: Word64 -> Word64
foreign import ccall unsafe "goldilocks_add" c_goldilocks_add :: Word64 -> Word64 -> Word64
foreign import ccall unsafe "goldilocks_sub" c_goldilocks_sub :: Word64 -> Word64 -> Word64
foreign import ccall unsafe "goldilocks_sqr" c_goldilocks_sqr :: Word64 -> Word64
foreign import ccall unsafe "goldilocks_mul" c_goldilocks_mul :: Word64 -> Word64 -> Word64
foreign import ccall unsafe "goldilocks_inv" c_goldilocks_inv :: Word64 -> Word64
foreign import ccall unsafe "goldilocks_div" c_goldilocks_div :: Word64 -> Word64 -> Word64
foreign import ccall unsafe "goldilocks_pow" c_goldilocks_pow :: Word64 -> CInt   -> Word64

neg :: Goldilocks -> Goldilocks
neg (MkGoldilocks k) = MkGoldilocks (c_goldilocks_neg k) 

add :: Goldilocks -> Goldilocks -> Goldilocks
add (MkGoldilocks a) (MkGoldilocks b) = MkGoldilocks (c_goldilocks_add a b) 

sub :: Goldilocks -> Goldilocks -> Goldilocks
sub (MkGoldilocks a) (MkGoldilocks b) = MkGoldilocks (c_goldilocks_sub a b) 

sqr :: Goldilocks -> Goldilocks
sqr (MkGoldilocks a)  = MkGoldilocks (c_goldilocks_sqr a) 

mul :: Goldilocks -> Goldilocks -> Goldilocks
mul (MkGoldilocks a) (MkGoldilocks b) = MkGoldilocks (c_goldilocks_mul a b) 

inv :: Goldilocks -> Goldilocks
inv (MkGoldilocks a)  = MkGoldilocks (c_goldilocks_inv a) 

div :: Goldilocks -> Goldilocks -> Goldilocks
div (MkGoldilocks a) (MkGoldilocks b) = MkGoldilocks (c_goldilocks_div a b) 

--------------------------------------------------------------------------------

pow_ :: Goldilocks -> Int -> Goldilocks
pow_ (MkGoldilocks x) e = MkGoldilocks $ c_goldilocks_pow x (fromIntegral e :: CInt)

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

