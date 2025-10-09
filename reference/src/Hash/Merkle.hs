
{-| Merkle tree construction (using a T=12 hash)

Conventions:

  * we use a "keyed compression function" to avoid collisions for different inputs

  * when hashing the bottom-most layer, we use the key bit 0x01

  * when hashing an odd layer, we pad with a single 0 hash and use the key bit 0x02

  * when building a tree on a singleton input, we apply 1 round of compression 
    (with key 0x03, as it's both the bottom-most layer and odd)

-}

{-# LANGUAGE StrictData #-}
module Hash.Merkle where

--------------------------------------------------------------------------------

import Data.Array
import Data.Bits

import Control.Monad
import Data.Binary

import Field.Goldilocks
import Field.Goldilocks.Extension ( FExt , F2(..) )
import Field.Encode

import Hash.Permutations
import Hash.Common
import Hash.Sponge

import Misc

--------------------------------------------------------------------------------

type Key = Int

theHashFunction :: Hash
theHashFunction = Monolith

--------------------------------------------------------------------------------

type FRow = Array Int F

hashFRow :: FRow -> Digest
hashFRow farr = hashFieldElems theHashFunction (elems farr)

hashFExt :: FExt -> Digest
hashFExt (F2 x y) = hashFieldElems theHashFunction [x,y]

{-
data LeafData
  = RowData   FRow
  | Singleton FExt
  deriving (Eq,Show)

instance FieldEncode LeafData where
  fieldEncode (RowData   farr) = elems farr
  fieldEncode (Singleton fext) = fieldEncode fext

hashLeafData :: LeafData -> Digest
hashLeafData leaf = case leaf of
  RowData   frow  -> hashFRow frow
  Singleton fext  -> hashFExt fext
-}

hashAny :: FieldEncode a => a -> Digest
hashAny = hashFieldElems theHashFunction . fieldEncode

--------------------------------------------------------------------------------

newtype MerkleCap 
  = MkMerkleCap { fromMerkleCap :: Array Int Digest }
  deriving (Eq,Show)

instance Binary MerkleCap where
  put = putSmallArray . fromMerkleCap
  get = MkMerkleCap <$> getSmallArray

instance FieldEncode MerkleCap where
  fieldEncode (MkMerkleCap arr) = concatMap fieldEncode (elems arr)

merkleCapSize :: MerkleCap -> Int
merkleCapSize (MkMerkleCap ds) = (arrayLength ds)

merkleCapLogSize :: MerkleCap -> Log2 
merkleCapLogSize (MkMerkleCap ds) = exactLog2__ (arrayLength ds)

-- | Computes the root of a Merkle cap 
--
-- (we implicitly assume that the cap was not the bottom layer)
merkleCapRoot :: MerkleCap -> Digest
merkleCapRoot (MkMerkleCap hashArray) =  
  case elems hashArray of
    []  -> error "merkleCapRoot: fatal: input is empty"
    [z] -> keyedCompress theHashFunction (nodeKey BottomLayer OddNode) z zeroDigest
    zs  -> go zs 
  where
    go :: [Digest] -> Digest
    go [x] = x
    go xs  = go (map (evenOddCompressPair OtherLayer) $ eiPairs xs)

--------------------------------------------------------------------------------

-- | Note: index 0 is the bottom (widest) layer
data MerkleTree a = MkMerkleTree 
  { _merkleTree   :: Array Int (Array Int Digest)
  , _merkleLeaves :: Array Int a -- LeafData
  }
  deriving Show

-- | @log2( number-of-leaves )@.
-- 
-- NOTE: this is one less than the actual number of layers! 
-- However it equals to the length of a Merkle path 
--
merkleTreeDepth :: MerkleTree a -> Log2
merkleTreeDepth = Log2 . merkleTreeDepth_

merkleTreeDepth_ :: MerkleTree a -> Int
merkleTreeDepth_ (MkMerkleTree outer _) = (b - a) where
  (a,b) = bounds outer

extractMerkleCap :: Log2 -> MerkleTree a -> MerkleCap
extractMerkleCap (Log2 capdepth) (MkMerkleTree layers _) = cap where
  (0,n) = bounds layers
  cap = MkMerkleCap (layers ! (n-capdepth))

treeBottomLayer :: MerkleTree a -> Array Int Digest
treeBottomLayer (MkMerkleTree outer _) = outer!0

--------------------------------------------------------------------------------

-- | Only the Merkle path (siblings)
newtype RawMerklePath
  = MkRawMerklePath [Digest]
  deriving (Eq,Show)

fromRawMerklePath :: RawMerklePath -> [Digest]
fromRawMerklePath (MkRawMerklePath ds) = ds

instance Binary RawMerklePath where
  put = putSmallList . fromRawMerklePath
  get = MkRawMerklePath <$> getSmallList

instance FieldEncode RawMerklePath where
  fieldEncode (MkRawMerklePath ds) = concatMap fieldEncode ds

data MerkleProof a = MkMerkleProof
  { _leafIndex   :: Int               -- ^ linear index of the leaf we prove, 0..dataSize-1
  , _leafData    :: a                 -- ^ the data on the leaf
  , _merklePath  :: RawMerklePath     -- ^ the path up the root
  , _dataSize    :: Int               -- ^ number of leaves in the tree
  }
  deriving (Eq,Show)

-- | Returns the leaf and Merkle path of the given leaf
extractMerkleProof :: MerkleTree a -> Int -> MerkleProof a
extractMerkleProof = extractMerkleProof' (Log2 0)

-- | Returns the leaf and Merkle path of the given leaf, up to a given Merkle cap depth
extractMerkleProof' :: Log2 -> MerkleTree a -> Int -> MerkleProof a
extractMerkleProof' (Log2 capDepth) tree@(MkMerkleTree outer leaves) idx = MkMerkleProof idx leaf path size where
  leaf  = leaves!idx
  size  = arrayLength (outer!0)
  depth = merkleTreeDepth_ tree
  path  = MkRawMerklePath $ takePrefix (worker depth idx)

  worker 0     0 = []
  worker 0     _ = error "extractMerkleProof: this should not happen"
  worker level j = this : worker (level-1) (shiftR j 1) where
    this = outer ! (depth - level) ! (j `xor` 1)

  takePrefix = take (depth - capDepth)

--------------------------------------------------------------------------------

calcMerkleTree' :: [Digest] -> [Array Int Digest]
calcMerkleTree' input = 
  case input of
    []  -> error "calcMerkleTree': input is empty"
    [z] -> [ singletonArray $ keyedCompress theHashFunction (nodeKey BottomLayer OddNode) z zeroDigest ]
    zs  -> map listToArray (go layerFlags zs) 
  where
    go :: [LayerFlag] -> [Digest] -> [[Digest]]
    go _ [x]     = [[x]]
    go (f:fs) xs = xs : go fs (map (evenOddCompressPair f) $ eiPairs xs)

calcMerkleTree :: FieldEncode a => [a] -> MerkleTree a
calcMerkleTree input = MkMerkleTree tree leafData where
  tree     = listToArray (calcMerkleTree' $ map hashAny input)
  leafData = listToArray input

calcArrayMerkleTree :: FieldEncode a => Array Int a -> MerkleTree a
calcArrayMerkleTree = calcMerkleTree . elems

-- | Applies a permutation of the rows.
--
-- We need the backward mapping (from Merkle tree indices to array indices)
calcArrayMerkleTree' :: FieldEncode a => (Int -> Int) -> Array Int a -> MerkleTree a
calcArrayMerkleTree' bwd arr = calcMerkleTree [ arr!(bwd i) | i<-[0..n-1] ] where
  n = arraySize arr

--------------------------------------------------------------------------------

reconstructMerkleRoot :: FieldEncode a => MerkleProof a -> Digest
reconstructMerkleRoot (MkMerkleProof idx leaf (MkRawMerklePath path) size) = digest where

  digest = go layerFlags size idx (hashAny leaf) path 
 
  go :: [LayerFlag] -> Int -> Int -> Digest -> [Digest] -> Digest
  go _      !sz  0 !h []      = h
  go (f:fs) !sz !j !h !(p:ps) = case (j.&.1, j==sz-1)  of
    (0, False) -> go fs sz' j' (evenOddCompressPair f $ Right (h,p)) ps
    (0, True ) -> go fs sz' j' (evenOddCompressPair f $ Left   h   ) ps
    (1, _    ) -> go fs sz' j' (evenOddCompressPair f $ Right (p,h)) ps
    where
      sz' = shiftR (sz+1) 1
      j'  = shiftR  j     1

--------------------------------------------------------------------------------

compress :: Hash -> Digest -> Digest -> Digest
compress which (MkDigest a b c d) (MkDigest p q r s) = extractDigest output where
  input  = listArray (0,11) [ a,b,c,d , p,q,r,s , 0,0,0,0 ]
  output = permute which input

keyedCompress ::  Hash -> Key -> Digest -> Digest -> Digest
keyedCompress which key (MkDigest a b c d) (MkDigest p q r s) = extractDigest output where
  k = fromIntegral key :: F
  input  = listArray (0,11) [ a,b,c,d , p,q,r,s , k,0,0,0 ]
  output = permute which input

--------------------------------------------------------------------------------

-- | bit masks
keyBottom = 1 :: Key
keyOdd    = 2 :: Key

--------------------------------------------------------------------------------

data LayerFlag  
  = BottomLayer           -- ^ it's the bottom (initial, widest) layer
  | OtherLayer            -- ^ it's not the bottom layer 
  deriving (Eq,Show)

data NodeParity
  = EvenNode              -- ^ it has 2 children
  | OddNode               -- ^ it has 1 child
  deriving (Eq,Show)

-- | Key based on the node type: 
-- 
-- > bit0 := 1 if bottom layer, 0 otherwise
-- > bit1 := 1 if odd, 0 if even
--
nodeKey :: LayerFlag -> NodeParity -> Key
nodeKey OtherLayer  EvenNode = 0x00
nodeKey BottomLayer EvenNode = 0x01
nodeKey OtherLayer  OddNode  = 0x02
nodeKey BottomLayer OddNode  = 0x03

evenOddCompressPair :: LayerFlag -> Either Digest (Digest,Digest) -> Digest
evenOddCompressPair !lf (Right (x,y)) = keyedCompress theHashFunction (nodeKey lf EvenNode) x y
evenOddCompressPair !lf (Left   x   ) = keyedCompress theHashFunction (nodeKey lf OddNode ) x zeroDigest

eiPairs :: [a] -> [Either a (a,a)]
eiPairs []         = []
eiPairs [x]        = Left   x    : []
eiPairs (x:y:rest) = Right (x,y) : eiPairs rest

layerFlags :: [LayerFlag]    
layerFlags = BottomLayer : repeat OtherLayer 

calcMerkleRoot' :: [Digest] -> Digest
calcMerkleRoot' input = 
  case input of
    []  -> error "calcMerkleRoot: input is empty"
    [z] -> keyedCompress theHashFunction (nodeKey BottomLayer OddNode) z zeroDigest
    zs  -> go layerFlags zs 
  where
    go :: [LayerFlag] -> [Digest] -> Digest
    go _      [x] = x
    go (f:fs) xs  = go fs (map (evenOddCompressPair f) $ eiPairs xs)

calcMerkleRoot :: FieldEncode a => [a] -> Digest
calcMerkleRoot = calcMerkleRoot' . map hashAny  -- hashLeafData

--------------------------------------------------------------------------------

calcMerkleCap' :: Log2 -> [Digest] -> MerkleCap
calcMerkleCap' (Log2 capDepth) input = 
  case input of
    []  -> error "calcMerkleRoot: input is empty"
    [z] -> MkMerkleCap $ listToArray $ [ keyedCompress theHashFunction (nodeKey BottomLayer OddNode) z zeroDigest ]
    zs  -> MkMerkleCap $ listToArray $ select $ go layerFlags zs 
  where
    go :: [LayerFlag] -> [Digest] -> [[Digest]]
    go _      [x] = [[x]]
    go (f:fs) xs  = xs : go fs (map (evenOddCompressPair f) $ eiPairs xs)

    select :: [[Digest]] -> [Digest]
    select xs = xs !! (length xs - 1 - capDepth)

calcMerkleCap :: FieldEncode a => Log2 -> [a] -> MerkleCap
calcMerkleCap capDepth = calcMerkleCap' capDepth . map hashAny  

--------------------------------------------------------------------------------
