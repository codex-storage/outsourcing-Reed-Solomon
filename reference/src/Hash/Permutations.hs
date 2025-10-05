
module Hash.Permutations where

--------------------------------------------------------------------------------

import qualified Hash.Monolith.Permutation as Monolith

import Hash.Common

--------------------------------------------------------------------------------

permute :: Hash -> State -> State
permute hash = case hash of
  Monolith -> Monolith.permutation

--------------------------------------------------------------------------------
