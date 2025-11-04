
module NTT.Subgroup where

--------------------------------------------------------------------------------

import Data.Bits

import Field.Goldilocks
import Misc

--------------------------------------------------------------------------------

-- | A cyclic subgroup (multiplicative)
data Subgroup g = MkSubgroup 
  { subgroupGen   :: !g          -- ^ the cyclic generator 
  , subgroupOrder :: !Int        -- ^ size of the subgroup
  }
  deriving (Eq,Show)

subgroupSize :: Subgroup g -> Int
subgroupSize = subgroupOrder

subgroupLogSize :: Subgroup g -> Log2
subgroupLogSize = exactLog2__ . subgroupSize

getSubgroup :: Log2 -> Subgroup F
getSubgroup log2@(Log2 n) 
  | n<0       = error "getSubgroup: negative logarithm"
  | n>32      = error "getSubgroup: we cannot fit a smooth subgroup larger than 2^32 into Goldilocks"
  | otherwise = MkSubgroup 
     { subgroupGen   = pow theSubgroupGenSize32 (2^(32-n)) 
     , subgroupOrder = exp2_ log2
     }

theSubgroupGenSize32 :: F
theSubgroupGenSize32 = pow theMultiplicativeGenerator expo where
  (expo,0) = Prelude.divMod (goldilocksPrime-1) (2^32)

-- | lists all elements of the (cyclic) subgroup
subgroupElems :: forall g. Num g => Subgroup g -> [g]
subgroupElems (MkSubgroup gen order) = go 1 order where
  go :: g -> Int -> [g]
  go _ 0 = []
  go a n = a : go (a * gen) (n-1)

halveSubgroup :: Num g => Subgroup g -> Subgroup g
halveSubgroup (MkSubgroup gen size) = if (size .&. 1 == 0)
  then MkSubgroup (gen * gen) (shiftR size 1)
  else error "halveSubgroup: subgroup order not divisible by two"

-- | Synonym for 'halveSubgroup'
squareSubgroup :: Num g => Subgroup g -> Subgroup g
squareSubgroup = halveSubgroup

-- | Generalization of 'squareSubgroup'
powSubgroup :: Subgroup F -> Int -> Subgroup F
powSubgroup (MkSubgroup gen order) k 
  | r /= 0     = error $ "subgroupPower: size of the subgroup is not divisible by " ++ show k
  | otherwise  = MkSubgroup (pow_ gen k) q
  where
    (q,r) = divMod order k

--------------------------------------------------------------------------------

data Coset g = MkCoset 
  { cosetGroup  :: !(Subgroup g)  
  , cosetOffset :: !g
  }
  deriving (Eq,Show)

cosetSize :: Coset g -> Int
cosetSize (MkCoset sg ofs) = subgroupSize sg

getCoset :: F -> Log2 -> Coset F
getCoset shift size = MkCoset (getSubgroup size) shift

squareCoset :: Num g => Coset g -> Coset g
squareCoset (MkCoset subgroup ofs) = MkCoset subgroup' ofs' where
  subgroup' = halveSubgroup subgroup
  ofs'      = ofs * ofs

powCoset :: Coset F -> Int -> Coset F
powCoset (MkCoset subgroup offset) expo = MkCoset (powSubgroup subgroup expo) (pow_ offset expo)

--------------------------------------------------------------------------------

-- | First @n@ powers of @g@
powersOf :: Num g => Int -> g -> [g]
powersOf n g = go 1 n where
  go  _  0 = []
  go !y !n = y : go (g*y) (n-1)

--------------------------------------------------------------------------------
