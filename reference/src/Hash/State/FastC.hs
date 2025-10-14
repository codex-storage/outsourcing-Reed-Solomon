
module Hash.State.FastC where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word

import Control.Monad
import System.IO.Unsafe

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable

import Field.Goldilocks

import Hash.Common

--------------------------------------------------------------------------------

type State = ForeignPtr F 

{-# NOINLINE listToStateIO #-}
listToStateIO :: Int -> [F] -> IO State
listToStateIO n xs = do
  fptr <- mallocForeignPtrArray n :: IO (ForeignPtr F)
  withForeignPtr fptr $ \ptr -> pokeArray ptr xs
  return fptr

listToState' :: Int -> [F] -> State
listToState' n xs = unsafePerformIO (listToStateIO n xs)

listToState :: Hash -> [F] -> State
listToState hash = listToState' (hashT hash)

zeroState' :: Int -> State
zeroState' n = listToState' n (replicate n 0)

zeroState :: Hash -> State
zeroState hash = zeroState' (hashT hash)

--------------------------------------------------------------------------------

{-# NOINLINE stateToListIO #-}
stateToListIO :: State -> IO [F]
stateToListIO fptr = do 
  withForeignPtr fptr $ \ptr -> do
    peekArray 12 ptr

stateToList :: State -> [F]
stateToList state = unsafePerformIO (stateToListIO state)

{-# NOINLINE extractDigestIO #-}
extractDigestIO :: State -> IO Digest
extractDigestIO fptr = 
  withForeignPtr fptr $ \ptr -> do
    a <- peek (ptr             )
    b <- peek (ptr `plusPtr`  8)
    c <- peek (ptr `plusPtr` 16)
    d <- peek (ptr `plusPtr` 24)
    return (MkDigest a b c d)

extractDigest :: State -> Digest
extractDigest state = unsafePerformIO (extractDigestIO state)

{-# NOINLINE overwriteIO #-}
overwriteIO :: [F] -> State -> IO State
overwriteIO xs src = do
  tgt <- mallocForeignPtrArray 12
  withForeignPtr src $ \ptr1 -> do
    withForeignPtr tgt $ \ptr2 -> do
      copyArray ptr2 ptr1 12
      pokeArray ptr2 xs
  return tgt

overwrite :: [F] -> State -> State
overwrite new old = unsafePerformIO (overwriteIO new old)

{-# NOINLINE addToStateIO #-}
addToStateIO :: [F] -> State -> IO State
addToStateIO xs src = do
  tgt <- mallocForeignPtrArray 12
  withForeignPtr src $ \ptr1 -> do
    withForeignPtr tgt $ \ptr2 -> do
      copyArray ptr2 ptr1 12
      forM_ (zip [0..] xs) $ \(i,x) -> do
        a <- peekElemOff ptr1 i
        pokeElemOff ptr2 i (a + x)
  return tgt

addToState :: [F] -> State -> State
addToState new old = unsafePerformIO (addToStateIO new old)

--------------------------------------------------------------------------------
