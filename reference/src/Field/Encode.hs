
-- | Encode stuff into field elements
--
-- This is to be able to hash the Fiat-Shamir transcript
--

module Field.Encode where

--------------------------------------------------------------------------------

import Data.Array

import Field.Goldilocks
import Field.Goldilocks.Extension ( FExt , F2(..) )

import Misc

--------------------------------------------------------------------------------

-- | This is to be able to hash stuff for the Fiat-Shamir transcript
class FieldEncode a where
  fieldEncode :: a -> [F]

instance FieldEncode F    where fieldEncode x        = [x]
instance FieldEncode F2   where fieldEncode (F2 x y) = [x,y]

instance FieldEncode Log2 where fieldEncode (Log2 k) = [fromIntegral k]
instance FieldEncode Int  where fieldEncode n        = [fromIntegral n]

instance FieldEncode a => FieldEncode [a] where 
  fieldEncode = concatMap fieldEncode 

instance FieldEncode a => FieldEncode (Array Int a) where 
  fieldEncode = concatMap fieldEncode . elems

--------------------------------------------------------------------------------
