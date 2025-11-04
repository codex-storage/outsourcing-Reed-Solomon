
-- | Bindings to a C implementation of the quadratic extension over the Goldilocks prime field
--
-- It's probably not significantly (if at all) faster than the naive Haskell combined 
-- with the fast Goldilocks base field operations, but the C versions should be useful 
-- for the vector operations, and this way we can test them easily.

{-# LANGUAGE ForeignFunctionInterface, BangPatterns, NumericUnderscores, TypeFamilies #-}
module Field.Goldilocks.Extension.BindC where

--------------------------------------------------------------------------------

import Prelude hiding ( div )
import qualified Prelude

import Data.Bits
import Data.Word
import Data.Ratio

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal

import System.Random
import System.IO.Unsafe

import Data.Binary
import Data.Binary.Get ( getWord64le )
import Data.Binary.Put ( putWord64le )

import Text.Printf

import Field.Goldilocks.Fast ( F , Goldilocks(..) )
import qualified Field.Goldilocks.Fast as Goldi

import Data.Flat
import Class.Field

--------------------------------------------------------------------------------

type FExt = F2

data F2 = F2 
  { real :: !F 
  , imag :: !F 
  }
  deriving (Eq)

--------------------------------------------------------------------------------

instance Binary F2 where
  put (F2 x y) = put x >> put y
  get = F2 <$> get <*> get
  
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

instance Flat F2 where
  sizeInBytes  _ = 16
  sizeInQWords _ = 2

  withFlat (F2 x y) action = allocaBytesAligned 16 8 $ \ptr -> do
    poke (castPtr ptr            ) x
    poke (castPtr ptr `plusPtr` 8) y
    action ptr 

  makeFlat ptr = do
    x <- peek (castPtr ptr            )
    y <- peek (castPtr ptr `plusPtr` 8)
    return (F2 x y)

--------------------------------------------------------------------------------

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

instance Ring FExt where
  zero        = Field.Goldilocks.Extension.BindC.zero
  one         = Field.Goldilocks.Extension.BindC.one
  isZero      = Field.Goldilocks.Extension.BindC.isZero
  isOne       = Field.Goldilocks.Extension.BindC.isOne
  square      = Field.Goldilocks.Extension.BindC.sqr
  power       = Field.Goldilocks.Extension.BindC.pow
  power_      = Field.Goldilocks.Extension.BindC.pow_

instance Field FExt

instance FiniteField FExt where
  fieldSize _ = (Goldi.goldilocksPrime ^ 2)
  rndIO       = randomIO

instance QuadraticExt FExt where
  type BaseField FExt = Goldi.F
  inject          = Field.Goldilocks.Extension.BindC.inj
  project         = Field.Goldilocks.Extension.BindC.proj
  scale           = Field.Goldilocks.Extension.BindC.scl
  quadraticPack   = Field.Goldilocks.Extension.BindC.pack
  quadraticUnpack = Field.Goldilocks.Extension.BindC.unpack

--------------------------------------------------------------------------------

zero, one, two :: F2
zero = F2 Goldi.zero Goldi.zero 
one  = F2 Goldi.one  Goldi.zero 
two  = F2 Goldi.two  Goldi.zero 

isZero, isOne :: F2 -> Bool
isZero (F2 r i) = Goldi.isZero r && Goldi.isZero i
isOne  (F2 r i) = Goldi.isOne  r && Goldi.isZero i

--------------------------------------------------------------------------------

{-# NOINLINE unaryOpIO #-}
unaryOpIO :: (Ptr Word64 -> Ptr Word64 -> IO ()) -> F2 -> IO F2
unaryOpIO c_action x = allocaBytesAligned 32 8 $ \ptr1 -> do
  let ptr2 = plusPtr ptr1 16
  poke (castPtr ptr1) x
  c_action ptr1 ptr2
  peek (castPtr ptr2)

{-# NOINLINE binaryOpIO #-}
binaryOpIO :: (Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()) -> F2 -> F2 -> IO F2
binaryOpIO c_action x y = allocaBytesAligned 48 8 $ \ptr1 -> do
  let ptr2 = plusPtr ptr1 16
  let ptr3 = plusPtr ptr1 32
  poke (castPtr ptr1) x
  poke (castPtr ptr2) y
  c_action ptr1 ptr2 ptr3
  peek (castPtr ptr3)

--------------------------------------------------------------------------------

inj :: F -> F2
inj r = F2 r 0 

proj :: F2 -> Maybe F
proj (F2 r i) = if Goldi.isZero i then Just r else Nothing

pack :: (F,F) -> F2
pack (r,i) = F2 r i

unpack :: F2 -> (F,F)
unpack (F2 r i) = (r,i)

--------------------------------------------------------------------------------

neg, sqr, inv :: F2 -> F2
neg x = unsafePerformIO (unaryOpIO c_goldilocks_ext_neg x)
sqr x = unsafePerformIO (unaryOpIO c_goldilocks_ext_sqr x)
inv x = unsafePerformIO (unaryOpIO c_goldilocks_ext_inv x)

add, sub, mul, div  :: F2 -> F2 -> F2
add x y = unsafePerformIO (binaryOpIO c_goldilocks_ext_add x y)
sub x y = unsafePerformIO (binaryOpIO c_goldilocks_ext_sub x y)
mul x y = unsafePerformIO (binaryOpIO c_goldilocks_ext_mul x y)
div x y = unsafePerformIO (binaryOpIO c_goldilocks_ext_div x y)

{-# NOINLINE sclIO #-}
sclIO :: F -> F2 -> IO F2
sclIO (MkGoldilocks s) x  = allocaBytesAligned 32 8 $ \ptr1 -> do
  let ptr2 = plusPtr ptr1 16
  poke (castPtr ptr1) x
  c_goldilocks_ext_scl s ptr1 ptr2
  peek (castPtr ptr2)

{-# NOINLINE powIO #-}
powIO :: F2 -> Int -> IO F2
powIO base expo = allocaBytesAligned 32 8 $ \ptr1 -> do
  let ptr2 = plusPtr ptr1 16
  poke (castPtr ptr1) base
  c_goldilocks_ext_pow ptr1 (fromIntegral expo :: CInt) ptr2
  peek (castPtr ptr2)

scl :: F -> F2 -> F2
scl s x = unsafePerformIO (sclIO s x)

pow_ :: F2 -> Int -> F2
pow_ b e = unsafePerformIO (powIO b e)

-- | NOTE: this is technically incorrect (it only works for small exponents), 
-- but we don't really care
pow :: F2 -> Integer -> F2
pow b e = pow_ b (fromInteger e)

--------------------------------------------------------------------------------

foreign import ccall unsafe "goldilocks_ext_neg" c_goldilocks_ext_neg :: Ptr Word64               -> Ptr Word64 -> IO ()
foreign import ccall unsafe "goldilocks_ext_add" c_goldilocks_ext_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "goldilocks_ext_sub" c_goldilocks_ext_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "goldilocks_ext_scl" c_goldilocks_ext_scl ::     Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "goldilocks_ext_sqr" c_goldilocks_ext_sqr :: Ptr Word64               -> Ptr Word64 -> IO ()
foreign import ccall unsafe "goldilocks_ext_mul" c_goldilocks_ext_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "goldilocks_ext_inv" c_goldilocks_ext_inv :: Ptr Word64               -> Ptr Word64 -> IO ()
foreign import ccall unsafe "goldilocks_ext_div" c_goldilocks_ext_div :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "goldilocks_ext_pow" c_goldilocks_ext_pow :: Ptr Word64 -> CInt       -> Ptr Word64 -> IO ()

--------------------------------------------------------------------------------

