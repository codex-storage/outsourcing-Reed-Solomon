
module Hash.Common where

--------------------------------------------------------------------------------

import Data.Array
import Data.Bits
import Data.Word

import Data.Binary

import Field.Goldilocks
import Field.Encode

--------------------------------------------------------------------------------

data Hash
  = Monolith
  deriving (Eq,Show)

hashT :: Hash -> Int
hashT hash = case hash of
  Monolith      -> 12

--------------------------------------------------------------------------------

newtype Rate 
  = Rate Int
  deriving (Eq,Ord,Show)

hashRate :: Hash -> Rate
hashRate hash = case hash of
  Monolith      -> Rate 8

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

data Digest 
  = MkDigest !F !F !F !F
  deriving (Eq,Show)

instance Binary Digest where
  put (MkDigest a b c d) = put a >> put b >> put c >> put d
  get = MkDigest <$> get <*> get <*> get <*> get

instance FieldEncode Digest where
  fieldEncode (MkDigest a b c d) = [a,b,c,d]
  
zeroDigest :: Digest
zeroDigest = MkDigest 0 0 0 0

extractDigest :: State -> Digest
extractDigest state = case elems state of 
  (a:b:c:d:_) -> MkDigest a b c d

listToDigest :: [F] -> Digest
listToDigest [a,b,c,d] = MkDigest a b c d

digestToList :: Digest -> [F]
digestToList (MkDigest a b c d) = [a,b,c,d]

--------------------------------------------------------------------------------

digestToWord64s :: Digest -> [Word64]
digestToWord64s (MkDigest a b c d) = [ fromF a, fromF b, fromF c, fromF d]

digestToBytes :: Digest -> [Word8]
digestToBytes = concatMap bytesFromWord64LE . digestToWord64s

--------------------------------------------------------------------------------

bytesFromWord64LE :: Word64 -> [Word8]
bytesFromWord64LE = go 0 where
  go 8  _  = []
  go !k !w = fromIntegral (w .&. 0xff) : go (k+1) (shiftR w 8)

bytesToWord64LE :: [Word8] -> Word64
bytesToWord64LE = fromInteger . bytesToIntegerLE

bytesToIntegerLE :: [Word8] -> Integer
bytesToIntegerLE = go where
  go []          = 0 
  go (this:rest) = fromIntegral this + 256 * go rest

--------------------------------------------------------------------------------
