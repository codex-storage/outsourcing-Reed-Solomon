
{-# LANGUAGE StrictData, RecordWildCards #-}
module FRI.Types where
  
--------------------------------------------------------------------------------

import Control.Monad
import Data.Binary
import qualified Data.ByteString.Lazy as L

import Field.Goldilocks
import Field.Goldilocks.Extension ( FExt , F2(..) )
import Field.Encode

import Hash.Merkle ( MerkleCap , MerkleProof , RawMerklePath , FRow )
import NTT.Subgroup
import NTT.Poly (Poly)
import Misc

--------------------------------------------------------------------------------

type Indent = Int

class Print a where
  showWithIndent    :: Indent -> a -> [String]
  showWithoutIndent :: a -> [String]

  showWithIndent indent what = indentLines indent (showWithoutIndent what)
  showWithoutIndent          = showWithIndent 0 

printWithIndent :: Print a => Indent -> a -> IO ()
printWithIndent indent what = putStrLn $ unlines (showWithIndent indent what)

justPrint :: Print a => a -> IO ()
justPrint = printWithIndent 0

indentLines :: Int -> [String] -> [String]
indentLines indent ls = map (prefix++) ls where prefix = replicate indent ' '

--------------------------------------------------------------------------------

-- | Reed-Solomon configuration
data RSConfig = MkRSConfig 
  { rsRateBits    :: Log2        -- ^ @r = -log2(rho)@
  , rsDataSize    :: Log2        -- ^ @n = log2(N)@
  , rsCosetShift  :: F           -- ^ the shift of the evaluation domain wrt. the subgroup
  }
  deriving (Eq,Show)

instance Binary RSConfig where
  put (MkRSConfig{..}) = do
    put rsRateBits  
    put rsDataSize  
    put rsCosetShift
  get = MkRSConfig <$> get <*> get <*> get
   
exampleRSConfig :: RSConfig
exampleRSConfig = MkRSConfig 8 3 theMultiplicativeGenerator

instance FieldEncode RSConfig where
  fieldEncode (MkRSConfig{..}) 
    =  fieldEncode rsRateBits
    ++ fieldEncode rsDataSize
    ++ fieldEncode rsCosetShift

rsEncodedSize :: RSConfig -> Log2
rsEncodedSize cfg = rsDataSize cfg + rsRateBits cfg

rsCosetSmall, rsCosetBig :: RSConfig -> Coset F
rsCosetSmall cfg = MkCoset (getSubgroup $ rsDataSize cfg                 ) (rsCosetShift cfg)
rsCosetBig   cfg = MkCoset (getSubgroup $ rsDataSize cfg + rsRateBits cfg) (rsCosetShift cfg)

instance Print RSConfig where
  showWithoutIndent (MkRSConfig{..}) = 
    [ " - rsBateRits   = " ++ show rsRateBits
    , " - rsDataSize   = " ++ show rsDataSize
    , " - rsCosetShift = " ++ show rsCosetShift
    ]

--------------------------------------------------------------------------------

-- | Folding arity
type Arity = Log2

newtype ReductionStrategy = MkRedStrategy 
  { fromReductionStrategy :: [Arity] 
  }
  deriving (Eq,Show)

instance Binary ReductionStrategy where
  put = putSmallList . fromReductionStrategy
  get = MkRedStrategy <$> getSmallList

instance FieldEncode ReductionStrategy where
  fieldEncode = fieldEncode . fromReductionStrategy

-- | Computes the sizes of the commit phase committed vectors + the size of the final polynomial
computeCommitPhaseFullSizes :: RSConfig -> ReductionStrategy -> ( [Log2] , Log2 )
computeCommitPhaseFullSizes rsConfig (MkRedStrategy arities) = go (rsEncodedSize rsConfig) arities where
  go :: Log2 -> [Log2] -> ( [Log2] , Log2 )
  go logN []     = ( [] , logN )
  go logN (a:as) = let (        sizes , final ) = go (logN-a) as
                   in  ( logN : sizes , final )

-- | As the leafs of the commit phase Merkle trees are in fact \"folding cosets\", 
-- the actual Merkle trees are smaller (less leaves)
computeCommitPhaseMerkleSizes :: RSConfig -> ReductionStrategy -> ( [Log2] , Log2 )
computeCommitPhaseMerkleSizes rsConfig (MkRedStrategy arities) = go (rsEncodedSize rsConfig) arities where
  go logN []     = ( [] , logN )
  go logN (a:as) = let (          sizes , final ) = go (logN-a) as
                   in  ( logN-a : sizes , final )

--------------------------------------------------------------------------------

-- | FRI configuration
data FriConfig = MkFriConfig 
  { friRSConfig          :: RSConfig             -- ^ Reed-Solomon configuration
  , friNColumns          :: Int                  -- ^ number of columns (batch FRI width)
  , friMerkleCapSize     :: Log2                 -- ^ size of the Merkle caps
  , friReductionStrategy :: ReductionStrategy    -- ^ folding arities
  , friNQueryRounds      :: Int                  -- ^ number of query rounds
  , friGrindingBits      :: Log2                 -- ^ grinding hardness
  }
  deriving (Eq,Show)

instance Binary FriConfig where
  put (MkFriConfig{..}) = do
    put friRSConfig          
    put friNColumns          
    put friMerkleCapSize     
    put friReductionStrategy 
    put friNQueryRounds      
    put friGrindingBits      
  get = MkFriConfig <$> get <*> get <*> get <*> get <*> get <*> get

instance Print FriConfig where
  showWithIndent indent (MkFriConfig{..}) = 
    [ " - friRSConfig\n"           ++ unlines1 (showWithIndent (indent+2) friRSConfig)
    , " - friNColumns          = " ++ show friNColumns         
    , " - friMerkleCapSize     = " ++ show friMerkleCapSize    
    , " - friReductionStrategy = " ++ show (map fromLog2 $ fromReductionStrategy friReductionStrategy)
    , " - friNQueryRounds      = " ++ show friNQueryRounds     
    , " - friGrindingBits      = " ++ show friGrindingBits     
    ]

-- instance FieldEncode ReductionStrategy where 
--   fieldEncode = concatMap fieldEncode

instance FieldEncode FriConfig where
  fieldEncode (MkFriConfig{..}) = concat
    [ fieldEncode friRSConfig          
    , fieldEncode friNColumns          
    , fieldEncode friMerkleCapSize     
    , fieldEncode friReductionStrategy 
    , fieldEncode friNQueryRounds      
    , fieldEncode friGrindingBits      
    ]

--------------------------------------------------------------------------------

data ReductionStrategyParams = MkRedStratPars
  { redStoppingDegree :: Log2             -- ^ stopping degree        
  , redFoldingArity   :: Log2             -- ^ default folding arity
  }
  deriving (Eq,Show)

-- | stop at degree 32 and folding arity 16
defaultReductionStrategyParams :: ReductionStrategyParams
defaultReductionStrategyParams = MkRedStratPars
  { redStoppingDegree = Log2 5            -- ^ stopping degree        
  , redFoldingArity   = Log2 4            -- ^ default folding arity
  }

findReductionStrategy :: ReductionStrategyParams -> RSConfig -> ReductionStrategy
findReductionStrategy (MkRedStratPars{..}) (MkRSConfig{..}) = MkRedStrategy $ worker (rsDataSize + rsRateBits) where
  worker k 
    | k <= redStoppingDegree                    = []
    | k >= redStoppingDegree + redFoldingArity  = redFoldingArity : worker (k - redFoldingArity)
    | otherwise                                 = [ k - redStoppingDegree ]

--------------------------------------------------------------------------------

-- | Only used in the verifier (but for debugging purposes only, also in the prover...)
data FriChallenges = MkFriChallenges
  { friAlpha          :: FExt          -- ^ column linear combination coefficient
  , friBetas          :: [FExt]        -- ^ folding step betas
  , friGrindResponse  :: F             -- ^ the PoW response (computed via Fiat-Shamir), which should have predetermined bit patterns
  , friQueryIndices   :: [Int]         -- ^ query indices
  }
  deriving (Eq,Show)

--------------------------------------------------------------------------------

data FriQueryStep = MkFriQueryStep
  { queryEvals      :: [FExt] 
  , queryMerklePath :: RawMerklePath
  }
  deriving (Eq,Show)

data FriQueryRound = MkFriQueryRound 
  { queryRow              :: FRow
  , queryInitialTreeProof :: RawMerklePath
  , querySteps            :: [FriQueryStep]
  }
  deriving (Eq,Show)

data FriProof = MkFriProof 
  { proofFriConfig       :: FriConfig          -- ^ the FRI configuration
  , proofCommitPhaseCaps :: [MerkleCap]        -- ^ commit phase Merkle caps
  , proofFinalPoly       :: Poly FExt          -- ^ the final polynomial in coefficient form
  , proofQueryRounds     :: [FriQueryRound]    -- ^ query rounds
  , proofPowWitness      :: F                  -- ^ witness showing that the prover did PoW
  }
  deriving (Eq,Show)

----------------------------------------
-- binary instances

friProofSizeInBytes :: FriProof -> Int
friProofSizeInBytes friProof = fromIntegral $ L.length (encode friProof)

instance Binary FriQueryStep where
  put (MkFriQueryStep{..}) = do
    putSmallList queryEvals
    put          queryMerklePath
  get =  MkFriQueryStep 
     <$> getSmallList
     <*> get

instance Binary FriQueryRound where
  put (MkFriQueryRound{..}) = do
    putSmallArray queryRow
    put           queryInitialTreeProof
    putSmallList  querySteps
  get =  MkFriQueryRound
     <$> getSmallArray
     <*> get
     <*> getSmallList

instance Binary FriProof where
  put (MkFriProof{..}) = do
    put          proofFriConfig       
    putSmallList proofCommitPhaseCaps 
    put          proofFinalPoly       
    putSmallList proofQueryRounds     
    put          proofPowWitness      
  get =  MkFriProof
     <$> get
     <*> getSmallList
     <*> get
     <*> getSmallList
     <*> get

--------------------------------------------------------------------------------
