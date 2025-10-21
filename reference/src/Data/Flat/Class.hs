

-- | Objects having flat (constant sized) representation in memory
--
-- Examples are: fixed size bigints, field elements, elliptic curve points
--

{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeFamilies, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Data.Flat.Class where

--------------------------------------------------------------------------------

import Data.Array
import Data.Int
import Data.Word
import Data.Proxy
import Data.Kind

import Control.Monad

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable

import System.IO
import System.IO.Unsafe

import Misc

--------------------------------------------------------------------------------

-- | This is kind of similar to @Storable@, but we expect the object
-- to be stored in some piece of continuous foreign memory.
class Flat a where
  -- | The size of the object in bytes
  sizeInBytes  :: Proxy a -> Int
  -- | The size of the object in 64-bit words 
  sizeInQWords :: Proxy a -> Int
  -- | Access to the raw data
  withFlat :: a -> (Ptr Word64 -> IO b) -> IO b
  -- | Create a new instance by copying the data from memory
  makeFlat :: Ptr Word64 -> IO a

{-
-- we assume that we are on a 64 bit machine and `Int = Int64s`
instance Flat Int where
  sizeInBytes  _ = 8
  sizeInQWords _ = 1
  withFlat x action = alloca $ \ptr -> poke (castPtr ptr) x >> action ptr
  makeFlat ptr      = peek (castPtr ptr)

instance Flat Word where
  sizeInBytes  _ = 8
  sizeInQWords _ = 1
  withFlat x action = alloca $ \ptr -> poke (castPtr ptr) x >> action ptr
  makeFlat ptr      = peek (castPtr ptr)
-}

instance Flat Int64 where
  sizeInBytes  _ = 8
  sizeInQWords _ = 1
  withFlat x action = alloca $ \ptr -> poke (castPtr ptr) x >> action ptr
  makeFlat ptr      = peek (castPtr ptr)

instance Flat Word64 where
  sizeInBytes  _ = 8
  sizeInQWords _ = 1
  withFlat x action = alloca $ \ptr -> poke ptr x >> action ptr
  makeFlat ptr      = peek ptr

--------------------------------------------------------------------------------

-- | temporary hack; TODO: fix this
newtype Bit 
  = Bit Word8
  deriving (Eq,Ord,Show,Storable)

boolToBit :: Bool -> Bit
boolToBit b = Bit (if b then 1 else 0)

bitToBool :: Bit -> Bool
bitToBool (Bit b) = (b /= 0)

-- | This is hack, but can be useful. TODO: figure out something to fix this
instance Flat Bit where
  sizeInBytes  _ = 1
  sizeInQWords _ = error "Flat/Bool/sizeInQWords: Bool is encoded as a single byte"
  withFlat (Bit x) action = alloca $ \ptr -> poke ptr x >> action (castPtr ptr)
  makeFlat ptr      = peek (castPtr ptr)

--------------------------------------------------------------------------------

makeFlatGeneric :: (ForeignPtr Word64 -> a) -> Int -> Ptr Word64 -> IO a
makeFlatGeneric wrap nwords srcPtr = do
  fptr <- mallocForeignPtrBytes (8*nwords)
  withForeignPtr fptr $ \tgtPtr -> copyBytes tgtPtr srcPtr (8*nwords)
  return (wrap fptr)

-- | Read out a list of words
peekFlat :: forall a. Flat a => a -> IO [Word64]
peekFlat what = withFlat what $ \ptr -> peekArray (sizeInQWords pxy) ptr where
  pxy = Proxy @a

-- | Create a new instance from a list of @Word64@-s
newFlat :: forall a. Flat a => [Word64] -> IO a
newFlat ws = do
  let n = sizeInQWords (Proxy @a)
  withArrayLen ws $ \m ptr -> if n == m
    then makeFlat ptr
    else error "newFlat: input has wrong length"

--------------------------------------------------------------------------------
