

-- | Flat arrays (stored linearly in a memory buffer)
-- 

{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeFamilies, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Data.Flat.Array where

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

import Data.Flat.Class
import Misc

--------------------------------------------------------------------------------

-- | A flat array of flat objects, represented as a continuous segment of 
-- foreign memory (not managed by the Haskell runtime). 
--
-- Note: the @Int@ means the number of objects in the array.
data FlatArray a 
  = MkFlatArray !Int !(ForeignPtr Word64)
  deriving Show

flatArrayLength :: FlatArray a -> Int
flatArrayLength (MkFlatArray n _) = n

flatArraySizeInBytes :: forall a. Flat a => FlatArray a -> Int
flatArraySizeInBytes (MkFlatArray n _) = n * sizeInBytes (Proxy @a)

withFlatArray :: FlatArray a -> (Int -> Ptr Word64 -> IO b) -> IO b
withFlatArray (MkFlatArray n fptr) action = do
  withForeignPtr fptr $ \ptr -> action n ptr

withFlatArrays :: [FlatArray a] -> ([(Int, Ptr Word64)] -> IO b) -> IO b
withFlatArrays list action = go list >>= action where
  go []                          = return []
  go (MkFlatArray n fptr : rest) = withForeignPtr fptr $ \ptr -> do
    args' <- go rest
    return ((n,ptr) : args')

unsafeCastFlatArray :: FlatArray a -> FlatArray b
unsafeCastFlatArray (MkFlatArray n fptr) = MkFlatArray n fptr

-- | Note: currently, this does a copy. Maybe we should refactor @Flat@ so that this not happen? 
{-# NOINLINE singletonArray #-}
singletonArray :: forall a. Flat a => a -> FlatArray a
singletonArray x = unsafePerformIO (singletonArrayIO x)

{-# NOINLINE singletonArrayIO #-}
singletonArrayIO :: forall a. Flat a => a -> IO (FlatArray a)
singletonArrayIO x = withFlat x $ \ptr -> do
  makeFlatGeneric (MkFlatArray 1) (sizeInQWords (Proxy @a)) ptr

-- TODO:
-- parallelWithFlatArray :: Int -> FlatArray a -> (Int -> Ptr Word64 -> IO b) -> IO [b]

-- | Extracts the @n@-th element
{-# NOINLINE peekFlatArray #-}
peekFlatArray :: Flat a => FlatArray a -> Int -> a
peekFlatArray arr j = unsafePerformIO $ peekFlatArrayIO arr j

{-# NOINLINE peekFlatArrayIO #-}
peekFlatArrayIO :: forall a. Flat a => FlatArray a -> Int -> IO a
peekFlatArrayIO arr j = do
  let s = sizeInBytes (Proxy @a)
  withFlatArray arr $ \_ ptr -> do
    makeFlat (plusPtr ptr (s*j))

--------------------------------------------------------------------------------

-- | Something which is a newtype containing a 'FlatArray' 
-- (for example: a dense univariate polynomial)
class WrappedArray (a :: Type) where
  type Element a :: Type
  wrapArray   :: FlatArray (Element a) -> a
  unwrapArray :: a -> FlatArray (Element a)

wrappedArrayLength :: WrappedArray arr => arr -> Int
wrappedArrayLength = flatArrayLength . unwrapArray

instance forall (a :: Type). WrappedArray (FlatArray a) where
  type Element (FlatArray a) = a
  wrapArray   = id
  unwrapArray = id

--------------------------------------------------------------------------------

-- | The first @m@ elements of a flat array (note: this operation is essentially free)
takeFlatArray :: Int -> FlatArray a -> FlatArray a
takeFlatArray m (MkFlatArray n fptr) 
  | m < 0      = error "takeFlatArray: negative input"
  | otherwise  = MkFlatArray (min n m) fptr

-- | Note: this does copying under the hood!
{-# NOINLINE dropFlatArray #-}
dropFlatArray :: Flat a => Int -> FlatArray a -> FlatArray a
dropFlatArray n arr 
  | n < 0      = error "dropFlatArray: negative input"
  | otherwise  = unsafePerformIO (dropFlatArrayIO n arr)

{-# NOINLINE dropFlatArrayIO #-}
dropFlatArrayIO :: forall a. Flat a => Int -> FlatArray a -> IO (FlatArray a)
dropFlatArrayIO k (MkFlatArray n fptr1) = do
  unless (k >= 0) $ fail "dropFlatArrayIO: negative input"
  let sz = sizeInQWords (Proxy @a) 
  let m  = max 0 (n-k)    
  let m1 = max 1 (n-k)                        -- hack, but i don't feel safe allocating 0 bytes
  fptr2 <- mallocForeignPtrArray (sz*m1)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      let src = plusPtr ptr1 (8*sz*k)
      when (m>0) $ copyBytes ptr2 src (8*sz*m)
  return (MkFlatArray m fptr2)

----------------------------------------

-- | Read a flat array from a raw binary file. The size of the file determines the length of the array.
readFlatArray :: forall a. Flat a => Proxy a -> FilePath -> IO (FlatArray a)
readFlatArray pxy fpath = do
  h <- openBinaryFile fpath ReadMode
  nbytes <- fromInteger <$> hFileSize h :: IO Int
  let (len,rem) = divMod nbytes (sizeInBytes pxy)   -- (Proxy @a))
  if rem /= 0 
    then do
      hClose h
      fail "readFlatArray: the input file has invalid size"
    else do
      fptr <- mallocForeignPtrBytes nbytes
      withForeignPtr fptr $ \ptr -> do
        hGetBuf h ptr nbytes
      hClose h
      return (MkFlatArray len fptr)

-- | Write a flat array into a raw binary file
writeFlatArray :: Flat a => FilePath -> FlatArray a -> IO ()
writeFlatArray fpath arr = do
  h <- openBinaryFile fpath WriteMode
  withFlatArray arr $ \_ ptr -> hPutBuf h ptr (flatArraySizeInBytes arr)
  hClose h

--------------------------------------------------------------------------------
-- * Pack \/ unpack flat arrays

-- | Create a flat array from elements. This is intended mostly for experimenting
-- and testing, as this is not a very efficient way of doing things.
--
{-# NOINLINE packFlatArray #-}
packFlatArray :: Flat a => Array Int a -> FlatArray a
packFlatArray arr = unsafePerformIO (packFlatArrayIO arr)

-- | Create a flat array from elements from a list.
packFlatArrayFromList :: Flat a => [a] -> FlatArray a
packFlatArrayFromList list = packFlatArrayFromList' (length list) list

-- | Create a flat array from elements from a list with a given size.
{-# NOINLINE packFlatArrayFromList #-}
packFlatArrayFromList' :: Flat a => Int -> [a] -> FlatArray a
packFlatArrayFromList' len list = unsafePerformIO (packFlatArrayFromListIO len list)

{-# NOINLINE unpackFlatArrayToList #-}
unpackFlatArrayToList :: Flat a => FlatArray a -> [a]
unpackFlatArrayToList flatArr = unsafePerformIO (unpackFlatArrayToListIO flatArr)

{-# NOINLINE unpackFlatArray #-}
unpackFlatArray :: Flat a => FlatArray a -> Array Int a
unpackFlatArray flatArr@(MkFlatArray len _) = unsafePerformIO $ do
  list <- unpackFlatArrayToListIO flatArr
  return $ listArray (0,len-1) list

--------------------------------------------------------------------------------
-- * Pack \/ unpack flat arrays in IO

{-# NOINLINE packFlatArrayIO #-}
packFlatArrayIO :: forall a. Flat a => Array Int a -> IO (FlatArray a)
packFlatArrayIO arr = do
  let (a,b) = bounds arr
  let n  = b-a+1
  packFlatArrayFromListIO n (elems arr)

{-# NOINLINE packFlatArrayFromListIO #-}
packFlatArrayFromListIO :: forall a. Flat a => Int -> [a] -> IO (FlatArray a)
packFlatArrayFromListIO n list = do
  let sz = sizeInBytes (Proxy @a) 
  fptr <- mallocForeignPtrBytes (n*sz)
  withForeignPtr fptr $ \arrPtr -> do
    forM_ (zip [0..n-1] list) $ \(j,x) -> do
      let tgt = plusPtr arrPtr (j*sz)
      withFlat x $ \src -> copyBytes tgt src sz
  return $ MkFlatArray n fptr    

{-# NOINLINE unpackFlatArrayToListIO #-}
unpackFlatArrayToListIO :: forall a. Flat a => FlatArray a -> IO [a]
unpackFlatArrayToListIO (MkFlatArray len fptr) = do
  let sz = sizeInBytes (Proxy @a) 
  withForeignPtr fptr $ \arrPtr -> do
    forM [0..len-1] $ \j -> do
      let src = plusPtr arrPtr (j*sz)
      makeFlat src

--------------------------------------------------------------------------------
