
module NTT.Tests where

--------------------------------------------------------------------------------

import Data.Array

import Control.Monad

import Class.Field
import Field.Goldilocks ( F ) 
import Misc

import Data.Flat as L
import NTT.Subgroup

import qualified NTT.Poly.Flat as F
import qualified NTT.FFT.Fast  as F

import qualified NTT.Poly.Naive as S
import qualified NTT.FFT.Slow   as S


--------------------------------------------------------------------------------

-- | Compare slow and fast FFT implementations to each other
compareFFTs :: Log2 -> IO Bool
compareFFTs logSize = do
  let size = exp2_ logSize
  let sg = getSubgroup logSize

  values1_s <- listToArray <$> replicateM size rndIO   :: IO (Array Int F)
  let values1_f = L.packFlatArray values1_s            :: L.FlatArray F
  let poly1_s = S.Poly      values1_s                  :: S.Poly F
  let poly1_f = F.mkPolyArr values1_s                  :: F.Poly F

  let values2_s = S.subgroupNTT sg poly1_s             :: Array Int F
  let values2_f = F.forwardNTT  sg poly1_f             :: L.FlatArray F

  let poly2_s = S.subgroupINTT sg values1_s            :: S.Poly F
  let poly2_f = F.inverseNTT   sg values1_f            :: F.Poly F

  let ok_ntt  = values2_s                == L.unpackFlatArray values2_f
  let ok_intt = S.polyCoeffArray poly2_s == F.coeffsArr poly2_f

  return (ok_ntt && ok_intt)

--------------------------------------------------------------------------------

runTests :: Int -> Log2 -> IO Bool
runTests n size = do
  oks <- replicateM n (compareFFTs size)
  return (and oks)

--------------------------------------------------------------------------------
