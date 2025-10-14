
{-# LANGUAGE StrictData, RecordWildCards #-}
module Outsource.Types where

--------------------------------------------------------------------------------

import FRI.Types

--------------------------------------------------------------------------------

-- | The type parameter is only there, because in the proof, we don't want to
-- repeat the FRI configuration (which is already included in the FRI proof). 
--
-- It's a bit ugly, but hey this is just a prototype anyway!
--
data OutsourceConfig' friconfig = MkOutsourceConfig
  { outsrcFriConfig   :: friconfig     -- ^ the FRI protocol configuration
  , outsrcKeepParity  :: Log2          -- ^ how much parity data to keep: Original data size times @2^(-k)@
  }
  deriving (Eq,Show)

type OutsourceConfigFull = OutsourceConfig' FriConfig
type OutsourceConfig_    = OutsourceConfig' ()

-- | The size of the rows (= number of columns in the data matrix)
outSrcNColumns :: OutsourceConfigFull -> Int
outSrcNColumns = friNColumns . outsrcFriConfig

--------------------------------------------------------------------------------

-- | Proof that the outsourcing of Reed-Solomon is done correctly.
--
-- This is checked against the original data Merkle root and RS-encoded Merkle root
data OutsourceProof = MkOutsourceProof
  { outsrcConfig     :: OutsourceConfig' ()      -- ^ we don't want to repeat the FRI configuration...
  , outsrcFriProof   :: FriProof                 -- ^ ...which is already included in the FRI proof
  , outsrcConnection :: ConnectionProof          -- ^ connect the original data to the parity data
  }
  deriving (Eq,Show)

data ConnectionProof = MkConnectionProof
  -- TODO

--------------------------------------------------------------------------------
