
-- | Stuff shared between the prover and the verifier

{-# LANGUAGE RecordWildCards #-}
module FRI.Shared where

--------------------------------------------------------------------------------

import Data.Array
import Data.Bits
import Data.Word

import Control.Monad.IO.Class
import System.Random

import Field.Goldilocks           ( F          )
import Field.Goldilocks.Extension ( FExt , scl )
import Field.Encode

import NTT
import Hash
import Misc

import Hash.Duplex.Monad hiding ( absorb )
import qualified Hash.Duplex.Monad as Duplex

import FRI.Types

--------------------------------------------------------------------------------

absorb :: FieldEncode a => a -> DuplexIO ()
absorb = Duplex.absorb . fieldEncode

--------------------------------------------------------------------------------

checkGrindBits :: Log2 -> F -> Bool
checkGrindBits (Log2 grindingBits) candidate = (fromF candidate .&. mask == 0) where
  mask = shiftL 1 grindingBits - 1 :: Word64

--------------------------------------------------------------------------------

type Idx = Int

generateQueryIndices :: Int -> Int -> DuplexIO [Idx]
generateQueryIndices nn nQueryRounds = do
  felts <- squeezeN nQueryRounds :: DuplexIO [F]
  return $ map (indexMapping nn) felts

-- | Note: the bias is extremely small with Goldilocks, if @nn@ is a power of two
indexMapping :: Int -> F -> Idx
indexMapping nn x 
  = fromIntegral 
  $ mod (fromF x :: Word64) (fromIntegral nn :: Word64)   

--------------------------------------------------------------------------------

-- | Maps a natural index to the matrix Merkle tree leaf index
-- 
-- (we have to reorder the leaves, so that the original data is a subtree, 
-- and the parity data consists of subtrees to)
--
fullDomainIndexMapFwd :: RSConfig -> Int -> Int
fullDomainIndexMapFwd (MkRSConfig{..}) naturalIdx = merkleIdx where
  mm = exp2_ rsDataSize 
  kk = exp2_ rsRateBits
  (q,j) = divMod naturalIdx kk
  merkleIdx = q + mm * j

-- | The inverse mapping
fullDomainIndexMapBwd :: RSConfig -> Int -> Int
fullDomainIndexMapBwd (MkRSConfig{..}) merkleIdx = naturalIdx where
  mm = exp2_ rsDataSize 
  kk = exp2_ rsRateBits
  (j,q) = divMod merkleIdx mm
  naturalIdx = q * kk + j

--------------------------------------------------------------------------------

