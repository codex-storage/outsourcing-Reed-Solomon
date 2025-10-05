
module FRI.Matrix where

--------------------------------------------------------------------------------

import Data.Array ( Array )
import Data.Array.IArray

import Misc

--------------------------------------------------------------------------------

type Vector a = Array  Int      a
type Matrix a = Array (Int,Int) a

-- type MatrixF   = Matrix F
-- type VectorF   = Vector F
-- type VectorExt = Vector FExt

vectorLength :: Array Int a -> Int
vectorLength vector = n+1 where
  (0,n) = bounds vector

matrixDimensions :: Matrix a -> (Int,Int)
matrixDimensions matrix = (n+1,m+1) where
  ((0,0),(n,m)) = bounds matrix

extractColumn :: Matrix a -> Int -> Vector a
extractColumn matrix j 
  | 0 <= j && j < m  = listArray (0,n-1) [ matrix!(i,j) | i<-[0..n-1] ]
  | otherwise        = error "extractColumn: column index out of range" 
  where
    (n,m) = matrixDimensions matrix

extractRow :: Matrix a -> Int -> Vector a
extractRow matrix i 
  | 0 <= i && i < n  = listArray (0,m-1) [ matrix!(i,j) | j<-[0..m-1] ]
  | otherwise        = error "extractRow: row index out of range" 
  where
    (n,m) = matrixDimensions matrix

matrixColumns :: Matrix a -> Array Int (Vector a)
matrixColumns matrix = listArray (0,m-1) [ extractColumn matrix j | j<-[0..m-1] ] where
  (n,m) = matrixDimensions matrix

matrixRows :: Matrix a -> Array Int (Vector a)
matrixRows matrix = listArray (0,n-1) [ extractRow matrix i | i<-[0..n-1] ] where
  (n,m) = matrixDimensions matrix

joinColumns :: Array Int (Vector a) -> Matrix a
joinColumns columns = array ((0,0),(n-1,m-1)) entries where
  m = arrayLength   columns
  n = vectorLength (columns!0)
  entries = [ ( (i,j) , (columns!j)!i ) | i<-[0..n-1] , j<-[0..m-1] ]

--------------------------------------------------------------------------------
