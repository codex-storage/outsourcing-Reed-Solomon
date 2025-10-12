
module Main where

--------------------------------------------------------------------------------

import Data.Array
import Text.Show.Pretty
import System.Random

import Data.Binary
import qualified Data.ByteString.Lazy as L

import Hash.Duplex.Monad
import FRI
import Misc

--------------------------------------------------------------------------------

cosetShift :: F
cosetShift = theMultiplicativeGenerator 

reductionStrategyParams :: ReductionStrategyParams 
reductionStrategyParams = MkRedStratPars
  { redStoppingDegree    = Log2 3
  , redFoldingArity      = Log2 2    
  }

rsConfig :: RSConfig
rsConfig = MkRSConfig
  { rsRateBits    = Log2 2    
  , rsDataSize    = Log2 8        
  , rsCosetShift  = cosetShift
  }

friConfig :: FriConfig
friConfig = MkFriConfig 
  { friRSConfig          = rsConfig
  , friNColumns          = 3     
  , friMerkleCapSize     = Log2 2
  , friReductionStrategy = findReductionStrategy reductionStrategyParams rsConfig
  , friNQueryRounds      = 2 -- 10
  , friGrindingBits      = Log2 5
  }

origCoset :: Coset F
origCoset = getCoset cosetShift (rsDataSize rsConfig)

ldeCoset :: Coset F
ldeCoset = getCoset cosetShift (rsDataSize rsConfig + rsRateBits rsConfig)

--------------------------------------------------------------------------------

testData :: Matrix F
testData = array ((0,0),(n-1,m-1)) (zip coords values) where
  n = exp2_ (rsDataSize  rsConfig )
  m =        friNColumns friConfig
  coords = [ (i,j) | i<-[0..n-1], j<-[0..m-1] ]    -- row-major
  values = take (n*m) $ genValues 31 55 107

ldeTestData :: Matrix F
ldeTestData = ldeEncodeMatrix rsConfig testData

testValues :: [F]
testValues = genValues 31 55 107

genValues :: F -> F -> F -> [F] 
genValues a b c = d : genValues a' b' c' where
  d  = a + 2*b + 3*c
  aa = a*a
  bb = b*b
  cc = c*c
  a' = aa + c*bb + c + 1003
  b' = a + bb - b*cc + 3137
  c' = aa - b + a*cc + 15222

--------------------------------------------------------------------------------

printSeparator :: IO ()
printSeparator = putStrLn "----------------------------------------"

main :: IO ()
main = do
  putStrLn "testMain (outsourcing Reed-Solomon with FRI)\n"
  printSeparator

  setStdGen (mkStdGen 1337)    -- make it deterministic
  -- setStdGen (mkStdGen 133756)

  justPrint friConfig
  printSeparator
  
  (commits, friProof) <- runDuplexIO_ (encodeAndProveFRI friConfig testData)
  pPrint commits
  pPrint friProof

  let lbs = encode friProof
  let friProof' = decode lbs
  putStrLn $ "size of the serialized proof = " ++ show (L.length lbs)
  putStrLn $ "could serialize proof and then load back unchanged = " ++ show (friProof == friProof')

  let vkey = MkFriVerifierKey
        { vkeyFriConfig = friConfig
        , vkeyMatrixCap = _ldeCommitment commits
        }

  ok <- runDuplexIO_ (verifyFRI vkey friProof)
  putStrLn $ "verify FRI succeed = " ++ show ok

--------------------------------------------------------------------------------
