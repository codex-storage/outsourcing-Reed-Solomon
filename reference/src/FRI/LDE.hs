
-- | Low-degree extension, that is, Reed-Solomon encoding of the data columns

{-# LANGUAGE RecordWildCards #-}
module FRI.LDE 
  ( module Field.Goldilocks          
  , module Field.Goldilocks.Extension
  , module FRI.Matrix
  , ldeEncodeMatrix
  , ldeEncodeColumn
  )
  where

--------------------------------------------------------------------------------

import Data.Array ( Array )
import Data.Array.IArray

import Field.Goldilocks           ( F    )
import Field.Goldilocks.Extension ( FExt )

import NTT
import Hash
import Misc

import FRI.Matrix
import FRI.Types

--------------------------------------------------------------------------------

-- | Reed-Solomon encode the columns of the input (data) matrix
ldeEncodeMatrix :: RSConfig -> Matrix F -> Matrix F
ldeEncodeMatrix rsConfig@(MkRSConfig{..}) dataMatrix 
  | n /= exp2_ rsDataSize  = error "ldeEncodeMatrix: input data column dimension is not compatible with the RS configuration" 
  | otherwise              = joinColumns ldeColumns
  where
    (n,m) = matrixDimensions dataMatrix
    ldeColumns = amap (ldeEncodeColumn rsConfig) (matrixColumns dataMatrix)

-- | Reed-Solomon encode a single column vector
ldeEncodeColumn :: RSConfig -> Vector F -> Vector F
ldeEncodeColumn rsConfig@(MkRSConfig{..}) dataVector 
  | n /= exp2_ rsDataSize  = error "ldeEncodeColumn: input data column dimension is not compatible with the RS configuration" 
  | otherwise              = ldeVector
  where
    n = vectorLength dataVector
    sg1 = getSubgroup  rsDataSize
    sg2 = getSubgroup (rsDataSize + rsRateBits)
    coset1 = MkCoset sg1 rsCosetShift
    coset2 = MkCoset sg2 rsCosetShift
    poly      =           cosetINTT coset1 dataVector
    ldeVector = asymmetricCosetNTT  coset2 poly

--------------------------------------------------------------------------------

