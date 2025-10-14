
-- | Quadratic extension over the Goldilocks field
--
-- We use the irreducble polynomial @x^2 - 7@ to be compatible with Plonky3

module Field.Goldilocks.Extension where

--------------------------------------------------------------------------------

import Prelude hiding ( div )

import Data.Bits
import Data.Ratio

import System.Random

import Foreign.Ptr
import Foreign.Storable

import Data.Binary

import Field.Goldilocks ( F )
import qualified Field.Goldilocks as Goldi

--------------------------------------------------------------------------------

type FExt = F2

data F2 = F2 
  { real :: !F 
  , imag :: !F 
  }
  deriving (Eq)

instance Binary F2 where
  put (F2 x y) = put x >> put y
  get = F2 <$> get <*> get
  
instance Show F2 where
  show (F2 r i) = "(" ++ show r ++ " + j * " ++ show i ++ ")"

instance Num F2 where
  fromInteger = inj . fromIntegral
  negate = neg
  (+)    = add
  (-)    = sub
  (*)    = mul
  abs    = id
  signum _ = inj 1

instance Fractional F2 where
  fromRational y = fromInteger (numerator y) `div` fromInteger (denominator y)
  recip  = inv
  (/)    = div

instance Random F2 where
  -- random :: RandomGen g => g -> (a, g) 
  random  g = let (x,g' ) = random g
                  (y,g'') = random g'
              in  (F2 x y, g'')
  randomR = error "randomR/F2: doesn't make any sense"

instance Storable F2 where
  peek ptr = do
    r <- peek (castPtr ptr) 
    i <- peek (castPtr ptr `plusPtr` 8)
    return (F2 r i)
  poke ptr (F2 r i) = do
    poke (castPtr ptr)             r
    poke (castPtr ptr `plusPtr` 8) i
  sizeOf    _ = 16
  alignment _ = 8

--------------------------------------------------------------------------------

zero, one, two :: F2
zero = F2 Goldi.zero Goldi.zero 
one  = F2 Goldi.one  Goldi.zero 
two  = F2 Goldi.two  Goldi.zero 

isZero, isOne :: F2 -> Bool
isZero (F2 r i) = Goldi.isZero r && Goldi.isZero i
isOne  (F2 r i) = Goldi.isOne  r && Goldi.isZero i

--------------------------------------------------------------------------------

inj :: F -> F2
inj r = F2 r 0 

neg :: F2 -> F2
neg (F2 r i) = F2 (negate r) (negate i)

add :: F2 -> F2 -> F2
add (F2 r1 i1) (F2 r2 i2) = F2 (r1 + r2) (i1 + i2)

sub :: F2 -> F2 -> F2
sub (F2 r1 i1) (F2 r2 i2) = F2 (r1 - r2) (i1 - i2)

scl :: F -> F2 -> F2
scl s (F2 r i) = F2 (s * r) (s * i)

sqrNaive :: F2 -> F2
sqrNaive (F2 r i) = F2 r3 i3 where
  r3 = r*r + 7 * i*i
  i3 = 2 * r*i

mulNaive :: F2 -> F2 -> F2
mulNaive (F2 r1 i1) (F2 r2 i2) = F2 r3 i3 where
  r3 = r1 * r2 + 7 * i1 * i2
  i3 = r1 * i2 +     i1 * r2

-- uses Karatsuba trick to have one less multiplications
mulKaratsuba :: F2 -> F2 -> F2
mulKaratsuba (F2 r1 i1) (F2 r2 i2) = F2 r3 i3 where
  u = r1*r2
  w = i1*i2
  v = (r1+i1)*(r2+i2)
  r3 = u + 7*w
  i3 = v - u - w

sqr :: F2 -> F2
sqr = sqrNaive

mul :: F2 -> F2 -> F2
mul = mulKaratsuba

--------------------------------------------------------------------------------
-- * inverse and division

-- | We can solve the equation explicitly.
--
-- > irred = x^2 + p*x + q
-- > (a*x + b) * (c*x + d) = (a*c)*x^2 + (a*d+b*c)*x + (b*d)
-- >                       = (a*d + b*c - a*c*p)*x + (b*d - a*c*q)
--
-- and then we want to solve
--
-- > b*d       - a*c*q == 1
-- > a*d + b*c - a*c*p == 0
--
-- which has the solution:
--
-- > c = - a       / (b^2 - a*b*p + a^2*q)  
-- > d = (b - a*p) / (b^2 - a*b*p + a^2*q)
--
-- Remark: It seems the denominator being zero would mean that our
-- defining polynomial is not irreducible.
--
-- Note: we can optimize for the common case p=0; and also for q=1.
--
inv :: F2 -> F2
inv (F2 b a) = F2 d c where
  denom = b*b - 7*a*a
  c = - a / denom
  d =   b / denom

div :: F2 -> F2 -> F2
div u v = mul u (inv v)

--------------------------------------------------------------------------------

pow_ :: F2 -> Int -> F2
pow_ x e = pow x (fromIntegral e)

pow :: F2 -> Integer -> F2
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
