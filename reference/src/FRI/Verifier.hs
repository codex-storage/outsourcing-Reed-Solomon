
{-# LANGUAGE RecordWildCards, StrictData #-}
module FRI.Verifier where

--------------------------------------------------------------------------------

import Data.Array
import Data.Bits
import Data.Word

import Control.Monad

import Field.Goldilocks           ( F          )
import Field.Goldilocks.Extension ( FExt , scl )
import Field.Encode

import NTT
import Hash
import Misc

import Hash.Duplex.Pure ( DuplexState )
import Hash.Duplex.Monad hiding ( absorb )
import qualified Hash.Duplex.Monad as Duplex

import FRI.Shared
import FRI.Types

--------------------------------------------------------------------------------

verifyFRI :: MerkleCap -> FriProof -> DuplexIO Bool
verifyFRI matrixCap friProof@(MkFriProof{..}) = do

  challenges <- computeFriChallenges matrixCap friProof

  duplexPPrint "verifier challenges" challenges

  return False

computeFriChallenges :: MerkleCap -> FriProof -> DuplexIO FriChallenges
computeFriChallenges matrixCap (MkFriProof{..}) = do

  absorb proofFriConfig                                            -- initialize Fiat-Shamir with the global parameters
  absorb matrixCap                                                 -- absorb the matrix hash
  alpha <- squeeze :: DuplexIO FExt                                -- row combining coefficient challenge alpha
  betas <- forM proofCommitPhaseCaps $ \cap -> do
    absorb cap                                                     -- commit to the (commit phase) Merkle cap
    beta <- squeeze :: DuplexIO FExt                               -- generate folding challenge beta
    return beta
  absorb proofFinalPoly                                            -- absorb the final final polynomial
  absorb proofPowWitness                                           -- absorb the grinding PoW witness
  powResponse  <- squeeze :: DuplexIO F                            -- generate the PoW response 
  queryIndices <- generateQueryIndices nn friNQueryRounds          -- generate query indices

  return $ MkFriChallenges
    { friAlpha         = alpha
    , friBetas         = betas
    , friGrindResponse = powResponse
    , friQueryIndices  = queryIndices
    }

  where

    MkFriConfig{..} = proofFriConfig
    MkRSConfig{..}  = friRSConfig

    nn = exp2_ (rsDataSize + rsRateBits)

--------------------------------------------------------------------------------
