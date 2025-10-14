
module Hash.State.Naive where

--------------------------------------------------------------------------------

import Data.Array
import Data.Bits
import Data.Word

import Data.Binary

import Field.Goldilocks
import Field.Encode

import Hash.Common

--------------------------------------------------------------------------------

type State = Array Int F

listToState' :: Int -> [F] -> State
listToState' n = listArray (0,n-1)

listToState :: Hash -> [F] -> State
listToState hash = listToState' (hashT hash)

zeroState' :: Int -> State
zeroState' n = listToState' n (replicate n 0)

zeroState :: Hash -> State
zeroState hash = zeroState' (hashT hash)

--------------------------------------------------------------------------------

stateToList :: State -> [F]
stateToList = elems

extractDigest :: State -> Digest
extractDigest state = case elems state of 
  (a:b:c:d:_) -> MkDigest a b c d

overwrite :: [F] -> State -> State
overwrite new old = listToState' 12 $ new ++ drop (length new) (elems old)

addToState :: [F] -> State -> State
addToState xs arr = listArray (0,11) $ zipWith (+) (xs ++ repeat 0) (elems arr)

--------------------------------------------------------------------------------

