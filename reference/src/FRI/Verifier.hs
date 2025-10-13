

{-# LANGUAGE RecordWildCards, StrictData #-}
module FRI.Verifier where

--------------------------------------------------------------------------------

import Data.Array
import Data.Bits
import Data.Word

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans    ( lift   )
import Control.Monad.IO.Class ( liftIO )

import Field.Goldilocks           ( F , theMultiplicativeGenerator )
import Field.Goldilocks.Extension ( FExt , scl , inj )
import Field.Encode

import NTT
import Hash
import Misc

import Hash.Duplex.Pure ( DuplexState )
import Hash.Duplex.Monad hiding ( absorb )
import qualified Hash.Duplex.Monad as Duplex

import FRI.Shared
import FRI.Types

--------------------------------------------------------------------------------

data FriVerifierKey = MkFriVerifierKey
  { vkeyFriConfig :: FriConfig
  , vkeyMatrixCap :: MerkleCap
  }
  deriving (Eq,Show)

--------------------------------------------------------------------------------

type Msg = String

indent :: String -> String
indent = unlines . map ("  " ++) . lines

prefixErrorMsg :: MonadError Msg m => Msg -> m a -> m a 
prefixErrorMsg prefix action = catchError action handler where
  handler msg = throwError (prefix ++ ":\n" ++ indent msg)

unlessEqual :: (Eq a, Show a, MonadError Msg m) => a -> a -> (String -> String -> Msg) -> m () 
unlessEqual x y mkMsg = if x == y 
  then return ()
  else throwError $ mkMsg (show x) (show y)

failUnless :: MonadError Msg m => Bool -> Msg -> m ()
failUnless ok msg = case ok of
  True  -> return ()
  False -> throwError msg

--------------------------------------------------------------------------------

liftToExceptT :: MonadError e m => Either e a -> m a
liftToExceptT ei = case ei of
  Left  err -> throwError err
  Right yyy -> return yyy

liftToExceptDuplexIO :: ExceptT e IO a -> ExceptT e (DuplexT IO) a 
liftToExceptDuplexIO = mapExceptT ioToDuplexIO

--------------------------------------------------------------------------------

verifyFRI :: FriVerifierKey -> FriProof -> DuplexIO Bool
verifyFRI friVKey friProof = do
  ei <- runExceptT (verifyFRI' friVKey friProof)
  case ei of
    Left msg -> do
      liftIO $ putStrLn msg
      return False
    Right () -> return True

verifyFRI' :: FriVerifierKey -> FriProof -> ExceptT Msg (DuplexT IO) ()
verifyFRI' friVKey@(MkFriVerifierKey{..}) friProof@(MkFriProof{..}) = do

  -- check the proof shape
  prefixErrorMsg "proof shape check failed" $ liftToExceptDuplexIO $ checkProofShape vkeyFriConfig friProof
  let MkFriConfig{..} = vkeyFriConfig
  
  -- compute challenges
  challenges <- lift $ computeFriChallenges vkeyMatrixCap friProof

  {- 
  duplexPPrint "verifier challenges" challenges    -- debugging
  -}
  
  -- check proof-of-work grinding
  unless (checkGrindBits friGrindingBits (friGrindResponse challenges)) $ throwError "grinding challenge didn't pass"

  -- check the query rounds
  liftToExceptDuplexIO $ 
    safeZipWithM_ (checkQueryRound friVKey challenges friProof) 
      (friQueryIndices challenges) proofQueryRounds
  
  return ()

--------------------------------------------------------------------------------

-- | Checks the \"initial tree proof\" (in Plonky2 lingo), and returns combined value
-- (initial \"upstream\" value)
checkInitialTreeProof :: FriVerifierKey -> FExt -> Idx -> FriQueryRound -> ExceptT String IO FExt
checkInitialTreeProof (MkFriVerifierKey{..}) alpha queryIdx (MkFriQueryRound{..}) = do
  let merkleProof = MkMerkleProof
        { _leafIndex   = fullDomainIndexMapFwd (friRSConfig vkeyFriConfig) queryIdx
        , _leafData    = queryRow              
        , _merklePath  = queryInitialTreeProof
        , _dataSize    = exp2_ (rsEncodedSize $ friRSConfig vkeyFriConfig)
        }
  let m = friNColumns vkeyFriConfig
  failUnless (m == length queryRow)                          $ "row length doesn't equal the expected"
  failUnless (checkMerkleCapProof vkeyMatrixCap merkleProof) $ "initial tree Merkle proof failed"
  let alphas   = powersOf m alpha
  let combined = sum (safeZipWith scl (elems queryRow) alphas)
  return combined

--------------------------------------------------------------------------------

checkQueryRound :: FriVerifierKey -> FriChallenges -> FriProof -> Idx -> FriQueryRound -> ExceptT String IO ()
checkQueryRound vkey@(MkFriVerifierKey{..}) challenges theFriProof iniQueryIdx queryRound@(MkFriQueryRound{..}) = do

  -- check the initial tree proof, and calculate the corresponding value
  iniUpstreamValue <- checkInitialTreeProof vkey (friAlpha challenges) iniQueryIdx queryRound

  let MkFriConfig{..} = vkeyFriConfig
  let arities = fromReductionStrategy friReductionStrategy 

  -- setup for the folding consistency checks
  let steps = 
        [ MkStepInfo
            { stepArity      = arity
            , stepBeta       = beta
            , stepTreeCap    = cap
            , stepEvals      = queryEvals      friQueryStep
            , stepMerklePath = queryMerklePath friQueryStep
            }

        | (arity, beta, friQueryStep, cap) <- safeZip4 
            arities 
            (friBetas challenges) 
            querySteps 
            (proofCommitPhaseCaps theFriProof)
        ]
  
  let iniArity = myHead arities
  let iniTreeCfg = MkTreeCfg 
        { _treeSize   = rsEncodedSize friRSConfig - iniArity
        , _cosetSize  = iniArity
        }
  let initialStepState = MkStepState
        { stateFullSize   = rsEncodedSize friRSConfig
        , stateShift      = theMultiplicativeGenerator
        , stateQueryIdx   = iniQueryIdx
        , stateQueryValue = iniUpstreamValue
        } 

  -- check the folding steps (evaluation Merkle proofs + upstream consistency check)
  finalState <- foldM checkQueryStep initialStepState steps

  -- check final polynomial against the final folded value
  let loc     = stateEvalLocation finalState
  let polyVal = polyEvalAt (proofFinalPoly theFriProof) (inj loc)
  unlessEqual polyVal (stateQueryValue finalState) $ \a b -> 
    "final polynomial evaluation " ++ a ++ " does not match final downstream value " ++ b 

  return ()

--------------------------------------------------------------------------------

-- | Note: treeSize + cosetSize = vector size (because the tree is over the cosets)
data TreeCfg = MkTreeCfg
  { _treeSize  :: Log2        -- ^ log size of the tree (whose leafs are cosets)
  , _cosetSize :: Log2        -- ^ size of the cosets
  }
  deriving (Eq,Show)

data TreePos = MkTreePos
  { posLeafIdx  :: Int        -- ^ which leaf of the commit phase tree (which is over cosets)
  , posCosetOfs :: Int        -- ^ which point within the coset
  }
  deriving (Eq,Show)

-- we use natural indexing
-- upstream coset: { 0 , T , 2T , ... (K-1)T } where T = treeSize (note: K*T = subgroup size) 
naturalIdxToTreePos :: TreeCfg -> Idx -> TreePos
naturalIdxToTreePos (MkTreeCfg treeSize arity) naturalIdx 
  | naturalIdx < 0    = error "naturalIdxToTreePos: negative input index"
  | naturalIdx >= nn  = error "naturalIdxToTreePos: out of range" 
  | otherwise         = MkTreePos leafIdx cosetOfs 
  where
    kk = exp2_  arity
    mm = exp2_  treeSize
    nn = exp2_ (treeSize + arity)
    leafIdx  = Prelude.mod naturalIdx mm      -- leafIdx  < treeSize
    cosetOfs = Prelude.div naturalIdx mm      -- cosetOfs < cosetSize

naturalIdxToFoldingCoset :: F -> TreeCfg -> Idx -> Coset F
naturalIdxToFoldingCoset shift treeCfg@(MkTreeCfg{..}) naturalIdx = MkCoset cosetSubgroup offset where
  MkTreePos{..} = naturalIdxToTreePos treeCfg naturalIdx
  fineSubgroup  = getSubgroup (_treeSize + _cosetSize)
  cosetSubgroup = getSubgroup              _cosetSize
  offset = shift * pow_ (subgroupGen fineSubgroup) (posLeafIdx)

--------------------------------------------------------------------------------

data StepInfo = MkStepInfo
  { stepArity      :: Log2               -- ^ folding arity
  , stepBeta       :: FExt               -- ^ folding coeff
  , stepTreeCap    :: MerkleCap          -- ^ commit phase Merkle cap
  , stepEvals      :: [FExt]             -- ^ coset evals (from the FRI proof)
  , stepMerklePath :: RawMerklePath      -- ^ coset Merkle proof (from the FRI proof)
  }
  deriving Show

data StepState = MkStepState
  { stateFullSize   :: Log2              -- ^ size of the vector (note: the Merkle tree is smaller as the leaves are cosets!)
  , stateShift      :: F                 -- ^ the global shift accumulates while folding 
  , stateQueryIdx   :: Idx               -- ^ current query index
  , stateQueryValue :: FExt              -- ^ corresponding evaluation
  } 
  deriving Show

stateEvalLocation :: StepState -> F
stateEvalLocation (MkStepState{..}) 
  = stateShift 
  * pow_ (subgroupGen subgroup) stateQueryIdx 
  where
    subgroup = getSubgroup stateFullSize

-- | Check a single query step and also do the folding
checkQueryStep :: StepState -> StepInfo -> ExceptT String IO StepState
checkQueryStep upstream@(MkStepState{..}) (MkStepInfo{..})  = do

  let upstreamIdx     = stateQueryIdx
  let treeCfg         = MkTreeCfg { _treeSize = stateFullSize - stepArity , _cosetSize = stepArity }
  let treePos         = naturalIdxToTreePos treeCfg stateQueryIdx
  let downstreamIdx   = posLeafIdx treePos
  
  let foldingCoset    = naturalIdxToFoldingCoset stateShift treeCfg stateQueryIdx
  let inverseDFT      = polyCoeffArray $ cosetINTTExt foldingCoset (listToArray stepEvals)

  let betas           = powersOf (exp2_ stepArity) stepBeta
  let downstreamValue = sum $ safeZipWith (*) betas (elems inverseDFT)

{-
  liftIO $ do
    debugPrint "upstreamIdx"    upstreamIdx
    debugPrint "downstreamIdx"  downstreamIdx
    debugPrint "treeCfg"        treeCfg
    debugPrint "treePos"        treePos
    debugPrint "evals"          stepEvals
    debugPrint "upstreamValue"  stateQueryValue
    debugPrint "downtreamValue" downstreamValue
-}

  -- check the upstream value against the opened coset
  unless (stateQueryValue == stepEvals !! (posCosetOfs treePos)) $ do
    throwError "upstream evaluation value does not match"

  -- check the Merkle proof of the opened coset
  let merkleProof = MkMerkleProof 
        { _leafIndex   = downstreamIdx           -- note: "accidentally" this is the same as the downstream index
        , _leafData    = stepEvals
        , _merklePath  = stepMerklePath
        , _dataSize    = exp2_ (stateFullSize - stepArity)
        }
  unless (checkMerkleCapProof stepTreeCap merkleProof) $ do
    throwError "coset evaluation Merkle proof failed"

  return $ MkStepState
    { stateFullSize   = stateFullSize - stepArity
    , stateShift      = pow_ stateShift (exp2_ stepArity)
    , stateQueryIdx   = downstreamIdx
    , stateQueryValue = downstreamValue
    } 
    

--------------------------------------------------------------------------------

{-
data FriChallenges = MkFriChallenges
  { friAlpha          :: FExt          -- ^ column linear combination coefficient
  , friBetas          :: [FExt]        -- ^ folding step betas
  , friGrindResponse  :: F             -- ^ the PoW response (computed via Fiat-Shamir), which should have predetermined bit patterns
  , friQueryIndices   :: [Int]         -- ^ query indices
  }
  deriving (Eq,Show)
-}

-- | This is straightforward enough: Reproduce the Fiat-Shamir challenges from the proof
computeFriChallenges :: MerkleCap -> FriProof -> DuplexIO FriChallenges
computeFriChallenges matrixCap (MkFriProof{..}) = do

  absorb proofFriConfig                                            -- initialize Fiat-Shamir with the global parameters
  absorb matrixCap                                                 -- absorb the matrix hash
  alpha <- squeeze :: DuplexIO FExt                                -- row combining coefficient challenge alpha
  betas <- forM proofCommitPhaseCaps $ \cap -> do
    absorb cap                                                     -- commit to the (commit phase) Merkle cap
    beta <- squeeze :: DuplexIO FExt                               -- generate folding challenge beta
    return beta
  absorb proofFinalPoly                                            -- absorb the final final polynomial
  absorb proofPowWitness                                           -- absorb the grinding PoW witness
  powResponse  <- squeeze :: DuplexIO F                            -- generate the PoW response 
  queryIndices <- generateQueryIndices nn friNQueryRounds          -- generate query indices

  return $ MkFriChallenges
    { friAlpha         = alpha
    , friBetas         = betas
    , friGrindResponse = powResponse
    , friQueryIndices  = queryIndices
    }

  where

    MkFriConfig{..} = proofFriConfig
    MkRSConfig{..}  = friRSConfig

    nn = exp2_ (rsDataSize + rsRateBits)

--------------------------------------------------------------------------------

-- | Checks if the \"shape\" of the proof (sizes of lists, arrays, Merkle caps, 
-- Merkle paths, etc etc) is the expected one
--
checkProofShape :: FriConfig -> FriProof -> ExceptT String IO ()
checkProofShape expectedFriConfig@(MkFriConfig{..}) (MkFriProof{..}) = 

  do
    -- check some global stuff
    failUnless configMatches  $ "FRI configuration of the proof does not match the expected one"    

    unlessEqual friNQueryRounds (length proofQueryRounds) $ \a b -> 
      "number of query rounds in the proof " ++ a ++ " does match the expected " ++ b

    unlessEqual (length proofCommitPhaseCaps) numberOfFolds $ \a b -> 
      "number of commit phase Merkle caps " ++ a ++ " does not match the expected " ++ b 

    unlessEqual (polySize proofFinalPoly) (exp2_ expectedFinalPolyLogDegree) $ \a b -> 
      "final polynomial size " ++ a ++ " does not match the expected " ++ b                  

    failUnless merkleCapSizes $ "commit phase Merkle cap sizes do not match the expected one"

{-
    failUnless configMatches  $ "FRI configuration of the proof does not match the expected one"    
    failUnless numberOfRounds $ "number of query rounds in the proof does match the expected one"   
    failUnless nCommitPhases  $ "number of commit phase Merkle caps does not match the expected one"
    failUnless finalPolySize  $ "final polynomial size does not match the expected"                 
    failUnless merkleCapSizes $ "commit phase Merkle cap sizes do not match the expected one"
-}

    -- check query rounds
    safeFlippedZipWithM_ [0..friNQueryRounds-1] proofQueryRounds $ \i queryRound@(MkFriQueryRound{..}) -> do
      prefixErrorMsg ("in query round #" ++ show i) $ do

        -- check opened row length
        unlessEqual (arrayLength queryRow) (friNColumns) $ \a b -> 
          "opened row size " ++ a ++ " does not match the expected " ++ b

        -- check initial tree Merkle proof path length
        unlessEqual (rawMerklePathLength2 queryInitialTreeProof) (rsEncodedSize friRSConfig - friMerkleCapSize) $ \a b -> 
          "initial tree Merkle path length " ++ a ++ " does not match the expected " ++ b 

        -- check query steps
        let (treeSizes, _finalPolyLogSize) = computeCommitPhaseMerkleSizes friRSConfig friReductionStrategy

        let nsteps = length arities

        safeFlippedZipWith4M_ [0..nsteps-1] arities treeSizes querySteps $ \j arity treeSize queryStep@(MkFriQueryStep{..}) -> do 
          prefixErrorMsg ("in query step #" ++ show j) $ do

            -- check opened folding coset size
            unlessEqual (length queryEvals) (exp2_ arity) $ \a b -> 
              "coset evaluation size " ++ a ++ " does not match the expected one " ++ b

            -- check coset Merkle proof path length
            unlessEqual (rawMerklePathLength2 queryMerklePath) (treeSize - friMerkleCapSize) $ \a b ->
              "Merkle path length " ++ a ++ " does not match the expected one " ++ b

  where
    MkRSConfig{..}             = friRSConfig
    MkRedStrategy arities      = friReductionStrategy
    numberOfFolds              = length arities
    expectedFinalPolyLogDegree = rsDataSize - sum arities

    configMatches    =  expectedFriConfig == proofFriConfig
    numberOfRounds   =  friNQueryRounds == length proofQueryRounds
    nCommitPhases    =  length proofCommitPhaseCaps == numberOfFolds
    finalPolySize    =  polySize proofFinalPoly == exp2_ expectedFinalPolyLogDegree
    merkleCapSizes   =  and [ merkleCapSize cap == exp2_ friMerkleCapSize | cap <- proofCommitPhaseCaps ]
   
{- 

-- RECALL THE FOLLOWING DEFINITIONS:

data FriConfig = MkFriConfig 
  { friRSConfig          :: RSConfig             -- ^ Reed-Solomon configuration
  , friNColumns          :: Int                  -- ^ number of columns (batch FRI width)
  , friMerkleCapSize     :: Log2                 -- ^ size of the Merkle caps
  , friReductionStrategy :: ReductionStrategy    -- ^ folding arities
  , friNQueryRounds      :: Int                  -- ^ number of query rounds
  , friGrindingBits      :: Log2                 -- ^ grinding hardness
  }

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

-}

--------------------------------------------------------------------------------
