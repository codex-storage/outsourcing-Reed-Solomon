
-- | Duplex sponge used for Fiat-Shamir challenges

{-# LANGUAGE StrictData, GeneralizedNewtypeDeriving #-}
module Hash.Duplex.Pure 
  ( DuplexState
  , duplexInitialState
  , Absorb(..)
  , Squeeze(..)
  , squeezeN
  , theHashFunction
  )
  where

--------------------------------------------------------------------------------

import Data.Array

import Field.Goldilocks           ( F )
import Field.Goldilocks.Extension ( FExt , F2(..) )
import Hash.Permutations
import Hash.Common

--------------------------------------------------------------------------------

theHashFunction :: Hash
theHashFunction = Monolith

--------------------------------------------------------------------------------

-- | Duplex sponge construction with overwrite mode
data DuplexState 
  = Absorbing { duplexOld :: State, duplexInp :: [F] } 
  | Squeezing { duplexOld :: State, duplexOut :: [F] }
  deriving (Eq,Show)

duplexInitialState :: State -> DuplexState
duplexInitialState state = Absorbing state [] 

overwrite :: [F] -> State -> State
overwrite new old = listToState theHashFunction $ new ++ drop (length new) (elems old)

duplex :: [F] -> State -> State
duplex inp old = permute theHashFunction (overwrite inp old)

extract :: State -> [F]
extract state = reverse $ take rate (elems state) where 
  rate = 8

freshSqueezing :: State -> DuplexState
freshSqueezing new = Squeezing new (extract new)

--------------------------------------------------------------------------------

absorbFelt :: F -> DuplexState -> DuplexState
absorbFelt x mode = 
  case mode of
    Squeezing old _   -> absorbFelt x (Absorbing old [])
    Absorbing old inp -> if length inp < rate
      then                Absorbing             old (inp ++ [x]) 
      else absorbFelt x $ Absorbing (duplex inp old) [] 
  where
    rate = 8

squeezeFelt :: DuplexState -> (F, DuplexState)
squeezeFelt mode =
  case mode of
    Squeezing old out -> case out of
      []     -> let new = permute theHashFunction old
                in  squeezeFelt $ freshSqueezing new 
      (y:ys) -> (y, Squeezing old ys)
    Absorbing old inp -> case inp of
      []     -> squeezeFelt $ freshSqueezing (permute theHashFunction old) 
      (x:xs) -> squeezeFelt $ freshSqueezing (duplex inp old) 

--------------------------------------------------------------------------------

class Absorb a where
  absorb :: a -> DuplexState -> DuplexState

instance Absorb F where
  absorb = absorbFelt

instance Absorb FExt where absorb (F2 a b) = absorb [a,b]

instance Absorb a => Absorb [a] where
  absorb []     = id
  absorb (x:xs) = absorb xs . absorb x

instance Absorb Digest where absorb h = absorb (digestToList h)

-- instance Absorb MerkleCap where
--   absorb (MkMerkleCap digests) = absorb digests

--------------------------------------------------------------------------------

class Squeeze a where
  squeeze :: DuplexState -> (a, DuplexState)

squeezeN :: Squeeze a => Int -> DuplexState -> ([a], DuplexState)
squeezeN 0 state0 = ([],state0)
squeezeN n state0 = let (x   , state1) = squeeze        state0 
                        (xs  , state2) = squeezeN (n-1) state1
                    in  (x:xs, state2)

instance Squeeze F where squeeze = squeezeFelt

instance Squeeze FExt where 
  squeeze state0 = 
    let (x, state1) = squeeze state0
        (y, state2) = squeeze state1
    in  (F2 x y, state2)

--------------------------------------------------------------------------------
