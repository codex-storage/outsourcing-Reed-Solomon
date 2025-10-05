
-- | The Monolith permutation with @t = 12@

{-# LANGUAGE Strict #-}
module Hash.Monolith.Permutation where

--------------------------------------------------------------------------------

import Data.Array (Array)
import Data.Array.IArray
import Data.Bits
import Data.Word

import Field.Goldilocks
import Hash.Monolith.Constants
import Hash.Common

--------------------------------------------------------------------------------

permutation :: State -> State
permutation 
  = foldr1 (.) (map monolithRound $ reverse [0..5])
  . linearDiffusion

monolithRound :: Int -> State -> State
monolithRound ridx = concrete ridx . bricks . bars

--------------------------------------------------------------------------------

sboxByte :: Word8 -> Word8
sboxByte y = rol1 $ y `xor` (rol1 ny .&. rol2 y .&. rol3 y) where
  ny   = complement y
  rol1 = flip rotateL 1
  rol2 = flip rotateL 2
  rol3 = flip rotateL 3

sboxField :: F -> F
sboxField = toF . bytesToWord64LE . map sboxByte . bytesFromWord64LE . fromF

bars :: State -> State
bars old = case splitAt 4 (elems old) of
  (four,eight) -> listToState' 12 (map sboxField four ++ eight)

bricks :: State -> State
bricks old = listToState' 12 $ zipWith (+) (0 : map sqr xs) xs where xs = elems old

concrete' :: [F] -> State -> State
concrete' rcs = listToState' 12 . zipWith (+) rcs . elems . linearDiffusion

concrete :: Int -> State -> State
concrete ridx = concrete' [ monolithRoundConstants ! (ridx,j) | j<-[0..11] ]

--------------------------------------------------------------------------------

circulantRow :: State
circulantRow = listToState' 12 [ 7, 23, 8, 26, 13, 10, 9, 7, 6, 22, 21, 8 ]

linearDiffusion :: State -> State
linearDiffusion old = listToState' 12 
  [ sum [ old!j * circulantRow!(mod (j-k) 12) | j<-[0..11] ]
  | k <- [0..11]
  ]

--------------------------------------------------------------------------------
