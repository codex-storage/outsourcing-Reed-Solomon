
{-# LANGUAGE StrictData, ScopedTypeVariables, PatternSynonyms #-}
module NTT.Poly.Flat where

import Prelude  hiding (div,quot,rem)
import GHC.Real hiding (div,quot,rem)

import Data.Bits
import Data.Word
import Data.List
import Data.Array

import Control.Monad

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.ForeignPtr

import System.Random
import System.IO.Unsafe

import Class.Field
import NTT.Subgroup
import Data.Flat as L

--------------------------------------------------------------------------------

newtype Poly a = MkPoly (L.FlatArray a)

pattern XPoly n arr = MkPoly (L.MkFlatArray n arr)

mkPoly :: Flat f => [f] -> Poly f
mkPoly = MkPoly . L.packFlatArrayFromList

mkPoly' :: Flat f => Int -> [f] -> Poly f
mkPoly' len xs = MkPoly $ L.packFlatArrayFromList' len xs

mkPolyArr :: Flat f => Array Int f -> Poly f
mkPolyArr = MkPoly . L.packFlatArray

mkPolyFlatArr :: L.FlatArray f -> Poly f
mkPolyFlatArr = MkPoly

coeffs :: Flat f => Poly f -> [f]
coeffs (MkPoly arr) = L.unpackFlatArrayToList arr

coeffsArr :: Flat f => Poly f -> Array Int f
coeffsArr (MkPoly arr) = L.unpackFlatArray arr

coeffsFlatArr :: Poly f -> L.FlatArray f
coeffsFlatArr (MkPoly flat) = flat

--------------------------------------------------------------------------------

