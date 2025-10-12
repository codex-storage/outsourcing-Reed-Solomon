
{-# LANGUAGE RecordWildCards, StrictData #-}
module FRI.Prover where

--------------------------------------------------------------------------------

import Data.Array
import Data.Bits
import Data.Word

import Control.Monad.IO.Class
import System.Random

import Text.Show.Pretty

import Field.Goldilocks           ( F          )
import Field.Goldilocks.Extension ( FExt , scl )
import Field.Encode

import NTT
import Hash
import Misc

import Hash.Duplex.Pure ( DuplexState )
import Hash.Duplex.Monad hiding ( absorb )
import qualified Hash.Duplex.Monad as Duplex

import FRI.LDE
import FRI.Matrix
import FRI.Shared
import FRI.Types

--------------------------------------------------------------------------------

data MerkleCommitments = MkMerkleCommitments 
  { _dataCommitment :: MerkleCap
  , _ldeCommitment  :: MerkleCap
  }
  deriving Show

encodeAndProveFRI :: FriConfig -> Matrix F -> DuplexIO (MerkleCommitments, FriProof)
encodeAndProveFRI friConfig@(MkFriConfig{..}) dataMatrix = 
  do
    absorb friConfig                                                   -- initialize Fiat-Shamir with the global parameters
    absorb matrixCap                                                   -- absorb the matrix hash
    alpha <- squeeze :: DuplexIO FExt                                  -- row combining coefficient challenge alpha
    let bigColumn = combineColumnsWith alpha ldeColumns                -- combined column
    let bigPoly   = interpolate bigCoset bigColumn                     -- initial polynomial
    (phases, finalPoly) <- repeatedlyFoldPoly 
        friConfig friReductionStrategy bigCoset bigPoly                -- compute commit phases 
    absorb finalPoly                                                   -- absorb the final final polynomial
    MkPoW powWitness powResponse <- performGrinding friGrindingBits    -- do the grinding
    queryIndices <- generateQueryIndices nn friNQueryRounds            -- generate query indices
    queries <- liftIO $ mapM (singleQueryRound phases) queryIndices    -- execute the query rounds

    -- only for debugging purposes
    let challenges = MkFriChallenges
          { friAlpha         = alpha
          , friBetas         = map commitPhaseBeta phases
          , friGrindResponse = powResponse
          , friQueryIndices  = queryIndices
          }
    duplexPPrint "challenges" challenges

    let friProof = MkFriProof 
          { proofFriConfig       = friConfig
          , proofCommitPhaseCaps = map commitPhaseMerkleCap phases
          , proofFinalPoly       = finalPoly
          , proofQueryRounds     = queries
          , proofPowWitness      = powWitness
          }

    let commits = MkMerkleCommitments 
          { _dataCommitment = origDataCap
          , _ldeCommitment  = matrixCap
          }

    return ( commits , friProof )
  where

    MkRSConfig{..} = friRSConfig

    origDataCap = calcMerkleCap friMerkleCapSize (elems $ matrixRows dataMatrix)

    ldeMatrix   = ldeEncodeMatrix friRSConfig dataMatrix
    ldeRows     = matrixRows    ldeMatrix
    ldeColumns  = matrixColumns ldeMatrix
    
    bigSubgroup = getSubgroup ldeSizeLog2
    bigCoset    = MkCoset bigSubgroup rsCosetShift     -- initial evaluation domain

    matrixTree  = calcArrayMerkleTree' (fullDomainIndexMapBwd friRSConfig) ldeRows :: MerkleTree FRow
    matrixCap   = extractMerkleCap friMerkleCapSize matrixTree
    nn          = arraySize (ldeColumns!0)
    ldeSizeLog2 = exactLog2__ nn
  
    -- interpolate from the LDE combined column into a smaller degree polynomial
    interpolate :: Coset F -> Vector FExt -> Poly FExt
    interpolate ldeCoset@(MkCoset ldeSubgroup shift) ldeVector = cosetINTTExt dataCoset dataVector where
      dataVector = extractSubgroupArray (exp2_ rsRateBits) ldeVector
      dataCoset  = MkCoset (powSubgroup ldeSubgroup expFactor) shift
      expFactor  = exp2_ rsRateBits

    singleQueryRound :: [CommitPhaseData] -> Idx -> IO FriQueryRound
    singleQueryRound phases queryIdx  
      | initialSanityOK = do
          steps <- go phases queryIdx
          return $ MkFriQueryRound 
            { queryRow              = _leafData   initialProof
            , queryInitialTreeProof = _merklePath initialProof
            , querySteps            = steps
            }
      | otherwise = do
          -- pPrint initialProof
          fail "initial tree Merkle proof check failed"
      where
        initialProof    = extractMerkleProof' friMerkleCapSize matrixTree $ fullDomainIndexMapFwd friRSConfig queryIdx
        initialSanityOK = checkMerkleCapProof (extractMerkleCap friMerkleCapSize matrixTree) initialProof

        go :: [CommitPhaseData] -> Idx -> IO [FriQueryStep]
        go []                             _   = return []
        go (MkCommitPhaseData{..} : rest) idx 
          | sanityOK = do
              restSteps <- go rest idx'
              return (thisStep : restSteps) 
          | otherwise = do
              -- pPrint proof
              fail "commit phase Merkle proof sanity check failed" 
          where
            intArity    = exp2_ commitPhaseArity
            domainSize' = Prelude.div (cosetSize commitPhaseDomain) intArity    -- N/K
            idx'        = Prelude.mod idx domainSize'                           -- idx' = idx mod (N/K)
            proof       = extractMerkleProof' friMerkleCapSize commitPhaseMerkleTree idx'
            sanityOK    = checkMerkleCapProof (extractMerkleCap friMerkleCapSize commitPhaseMerkleTree) proof
            cosetValues = elems (_leafData proof)
            thisStep    = MkFriQueryStep
              { queryEvals      = cosetValues
              , queryMerklePath = _merklePath proof
              }

--------------------------------------------------------------------------------
-- * Grinding

data ProofOfWork = MkPoW 
  { powWitness  :: F
  , powResponse :: F
  }
  deriving (Eq,Show)

performGrinding :: Log2 -> DuplexIO ProofOfWork
performGrinding grindingBits = 
  do
    oldState <- unsafeGetInnerState
    worker oldState
  where
    worker :: DuplexState -> DuplexIO ProofOfWork
    worker origState = do
      witnessCandidate <- liftIO randomIO :: DuplexIO F 
      absorb witnessCandidate
      responseCandidate <- squeeze 
      case checkGrindBits grindingBits responseCandidate of
        True  -> do
          -- duplexPrint powCandidate
          return $ MkPoW 
            { powWitness  = witnessCandidate 
            , powResponse = responseCandidate
            }
        False -> do
          unsafeSetInnerState origState
          worker origState

--------------------------------------------------------------------------------
-- * Combining columns

combineColumnsWith :: FExt -> Array Int (Array Int F) -> Array Int FExt
combineColumnsWith alpha columns = listArray (0,n-1) list where
  m = arraySize  columns
  n = arraySize (columns!0)
  alphas = powersOf m alpha
  row i  = [ (columns!j)!i | j<-[0..m-1] ]
  list   = [ sum (safeZipWith scl (row i) alphas) | i<-[0..n-1] ]

--------------------------------------------------------------------------------
-- * Folding

data CommitPhaseData = MkCommitPhaseData 
  { commitPhasePoly       :: Poly FExt                      -- ^ the polynomial before folding
  , commitPhaseDomain     :: Coset F                        -- ^ evaluation domain
  , commitPhaseArity      :: Log2                           -- ^ the folding arity
  , commitPhaseBeta       :: FExt                           -- ^ folding coefficient beta
  , commitPhaseMerkleTree :: MerkleTree (Array Int FExt)    -- ^ Merkle tree over the folded cosets
  , commitPhaseMerkleCap  :: MerkleCap                      -- ^ commit phase Merkle cap
  }
  deriving Show

-- | Starting from a polynomial of degree @N-1@, we repeatedly 
-- 
-- * evaluate it on the corresponding evaluation domain (coset)
--
-- * commit to the values with a Merkle tree
--
-- * generate a folding challenge (after absorbing the Merkle commitment)
--
-- * fold the polynomial and also the domain
--

repeatedlyFoldPoly :: FriConfig -> ReductionStrategy -> Coset F -> Poly FExt -> DuplexIO ( [CommitPhaseData] , Poly FExt )
repeatedlyFoldPoly (MkFriConfig{..}) (MkRedStrategy arities) domain poly = go arities domain poly where
  go []           domain poly = return ( [] , poly )
  go (arity:rest) domain poly = do
    let intArity = exp2_ arity                                  -- size of the folded coset
    let values   = asymmetricCosetNTTExt domain poly            -- values on the evaluation domain
    let stride   = Prelude.div (cosetSize domain) intArity      -- folding coset stride
    let cosets   = untangleArray' stride values                 -- cosets
    let tree     = calcArrayMerkleTree cosets                   -- Merkle tree on the top of the cosets
    let cap      = extractMerkleCap friMerkleCapSize tree
    absorb cap                                                  -- commit to the Merkle cap
    beta <- squeeze :: DuplexIO FExt                            -- generate folding challenge beta
    -- duplexPrint "beta" beta
    let thisPhase = MkCommitPhaseData 
          { commitPhasePoly       = poly
          , commitPhaseDomain     = domain
          , commitPhaseArity      = arity
          , commitPhaseBeta       = beta
          , commitPhaseMerkleTree = tree
          , commitPhaseMerkleCap  = cap
          }
    let poly'    = foldPoly arity beta poly
    -- duplexPutStrLn ""
    -- duplexPrint "poly"  poly
    -- duplexPrint "poly'" poly'
    let domain'  = powCoset domain intArity
    (phases, final) <- go rest domain' poly'
    return ( thisPhase:phases , final ) 

{-
repeatedlyFoldPoly :: [(Arity,FExt)] -> Poly FExt -> ( [Poly FExt] , Poly FExt )
repeatedlyFoldPoly = go where
  go []                  poly = ( []          , poly  )
  go ((arity,beta):rest) poly = ( poly:phases , final ) where
    poly' = foldPoly arity beta poly
    (phases, final) = go rest poly'
-}

foldPoly :: Arity -> FExt -> Poly FExt -> Poly FExt
foldPoly arity beta (Poly poly) 
  | r /= 0    = error $ "foldPoly: input polynomial's size " ++ show bigSize ++ " is not divisible by the arity " ++ show intArity
  | otherwise = Poly foldedPoly
  where
    intArity  = exp2_ arity
    bigSize   = arraySize poly
    smallSize = q
    (q,r)  = divMod bigSize intArity
    pieces = untangleArray' intArity poly 
    foldedPoly = makeArray q $ \i -> sum (safeZipWith (*) betaCoeffs [ (pieces!j)!i | j<-[0..intArity-1] ])
    betaCoeffs = powersOf intArity beta

--------------------------------------------------------------------------------
