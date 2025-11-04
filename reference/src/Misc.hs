
{-# LANGUAGE ScopedTypeVariables #-}
module Misc where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Array
import Data.List

import qualified Data.Set as Set ; import Data.Set (Set)

import Control.Monad
import Data.Binary

import Debug.Trace

--------------------------------------------------------------------------------
-- * Debug

debug_ :: Show a => a -> b -> b
debug_ x y = trace (">>> " ++ show x) y

debug :: Show a => String -> a -> b -> b
debug n x y = trace (">>> " ++ n ++ " = " ++ show x) y

debugPrint :: Show a => String -> a -> IO ()
debugPrint n x = putStrLn ("> " ++ n ++ " = " ++ show x)

--------------------------------------------------------------------------------
-- * Integers

isEven :: Integer -> Bool
isEven n = (n .&. 1) == 0 

isOdd :: Integer -> Bool
isOdd n = (n .&. 1) /= 0 

--------------------------------------------------------------------------------
-- * Strings

-- | The difference from 'unlines' is that this one doesn't add a final newline
unlines1 :: [String] -> String
unlines1 = intercalate "\n" 

--------------------------------------------------------------------------------
-- * Log2

newtype Log2 
  = Log2 Int
  deriving (Eq,Ord,Show,Num)

fromLog2 :: Log2 -> Int
fromLog2 (Log2 k) 
  | k >=0      = k
  | otherwise  = error "fromLog2: negative exponent"

exp2 :: Log2 -> Integer
exp2 (Log2 k) 
  | k >= 0     = shiftL 1 k
  | otherwise  = error "exp2: negative exponent"

exp2_ :: Log2 -> Int
exp2_ (Log2 k) 
  | k >= 0     = shiftL 1 k
  | otherwise  = error "exp2_: negative exponent"

-- | Smallest integer @k@ such that @2^k@ is larger or equal to @n@
ceilingLog2 :: Integer -> Log2
ceilingLog2 = Log2 . wrapper where
  wrapper 0 = 0
  wrapper n = 1 + go (n-1) where
    go 0 = -1
    go k = 1 + go (shiftR k 1)

exactLog2 :: Integer -> Maybe Log2
exactLog2 n = if re == n then Just log2 else Nothing where
  log2 = ceilingLog2 n
  re   = exp2 log2

exactLog2_ :: Integer -> Log2
exactLog2_ n = case exactLog2 n of
  Just log2 -> log2
  Nothing   -> error "exactLog2_: not a power of two"

exactLog2__ :: Int -> Log2
exactLog2__ = exactLog2_ . fromIntegral

--------------------------------------------------------------------------------
-- * Lists

-- | just to avoid the annoying GHC warning
myHead :: [a] -> a
myHead (x:_) = x
myHead []    = error "myHead: empty list"

safeZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
safeZipWith f = go where
  go []     []     = []
  go (x:xs) (y:ys) = f x y : go xs ys
  go _      _      = error "safeZipWith: incompatible lengths"

safeZipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d] 
safeZipWith3 f = go where
  go []     []     []     = []
  go (x:xs) (y:ys) (z:zs) = f x y z : go xs ys zs 
  go _      _      _      = error "safeZipWith3: incompatible lengths"

safeZipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
safeZipWith4 f = go where
  go []     []     []     []     = []
  go (x:xs) (y:ys) (z:zs) (w:ws) = f x y z w : go xs ys zs ws
  go _      _      _      _      = error "safeZipWith4: incompatible lengths"

safeZip  = safeZipWith  (,)
safeZip3 = safeZipWith3 (,,)
safeZip4 = safeZipWith4 (,,,)

interleave :: [a] -> [a] -> [a]
interleave (x:xs) (y:ys) = x:y:interleave xs ys
interleave []     []     = []
interleave _      _      = error "interleave: expecting input lists of the same length"

partitionIntoChunks :: Int -> [a] -> [[a]]
partitionIntoChunks k = go where
  go [] = []
  go xs = take k xs : go (drop k xs)

nubOrd :: Ord a => [a] -> [a]
nubOrd = worker Set.empty where
  worker _ [] = []
  worker s (x:xs) 
    | Set.member x s = worker s xs
    | otherwise      = x : worker (Set.insert x s) xs

--------------------------------------------------------------------------------
-- * Monads

-- WTF, this is not in @base@ but 'Maybe' is?!
instance MonadFail (Either String) where
  fail = Left

safeZipWithM :: forall m a b c. MonadFail m => (a -> b -> m c) -> [a] -> [b] -> m [c]
safeZipWithM f xs ys = go xs ys where
  go :: [a] -> [b] -> m [c]
  go (x:xs) (y:ys) = do
    z  <- f  x  y
    zs <- go xs ys
    return (z:zs)
  go []     []     = return []
  go _      _      = fail "safeZipWithM: incompatible input sizes"

safeZipWith3M :: MonadFail m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
safeZipWith3M f xs ys zs = go xs ys zs where
  go (x:xs) (y:ys) (z:zs) = do
    w  <- f  x  y  z
    ws <- go xs ys zs
    return (w:ws)
  go []     []     []     = return []
  go _      _      _      = fail "safeZipWith3M: incompatible input sizes"

safeZipWith4M :: MonadFail m => (a -> b -> c -> d -> m e) -> [a] -> [b] -> [c] -> [d] -> m [e]
safeZipWith4M f xs ys zs us = go xs ys zs us where
  go (x:xs) (y:ys) (z:zs) (u:us) = do
    w  <- f  x  y  z  u 
    ws <- go xs ys zs us
    return (w:ws)
  go []     []     []     []     = return []
  go _      _      _      _      = fail "safeZipWith4M: incompatible input sizes"

safeZipWithM_ :: MonadFail m => (a -> b -> m c) -> [a] -> [b] -> m () 
safeZipWithM_ f xs ys = void (safeZipWithM f xs ys)

safeZipWith3M_ :: MonadFail m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m ()
safeZipWith3M_ f xs ys zs = void (safeZipWith3M f xs ys zs)

safeZipWith4M_ :: MonadFail m => (a -> b -> c -> d -> m e) -> [a] -> [b] -> [c] -> [d] -> m ()
safeZipWith4M_ f xs ys zs us = void (safeZipWith4M f xs ys zs us)

safeFlippedZipWithM_ :: MonadFail m => [a] -> [b] -> (a -> b -> m c) -> m () 
safeFlippedZipWithM_ xs ys f = safeZipWithM_ f xs ys

safeFlippedZipWith3M_ :: MonadFail m => [a] -> [b] -> [c] -> (a -> b -> c -> m d) -> m ()
safeFlippedZipWith3M_ xs ys zs f = safeZipWith3M_ f xs ys zs

safeFlippedZipWith4M_ :: MonadFail m =>[a] -> [b] -> [c] -> [d] -> (a -> b -> c -> d -> m e) -> m ()
safeFlippedZipWith4M_ xs ys zs us f = safeZipWith4M_ f xs ys zs us

--------------------------------------------------------------------------------
-- * Arrays

singletonArray :: a -> Array Int a
singletonArray x = listArray (0,0) [x]

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, length xs - 1) xs

makeArray :: Int -> (Int -> a) -> Array Int a
makeArray n fun = listArray (0,n-1) [ fun i | i<-[0..n-1] ]

arrayLength :: Array Int a -> Int
arrayLength arr = b - a + 1 where (a,b) = bounds arr

-- | Synonym for 'arrayLength'
arraySize :: Array Int a -> Int
arraySize = arrayLength

-- | Returns the default value when out of range
safeIndex :: a -> Array Int a -> Int -> a
safeIndex def arr j 
  | j < a      = def
  | j > b      = def
  | otherwise  = arr!j
  where
    (a,b) = bounds arr

takeArray :: Int -> Array Int a -> Array Int a
takeArray k arr = listArray (0,k-1) $ take k (elems arr)

dropArray :: Int -> Array Int a -> Array Int a
dropArray k arr = listArray (0,n1-k) [ arr!(k+j) | j<-[0..n1-k] ] where
  (0,n1) = bounds arr

appendArrays :: Array Int a -> Array Int a -> Array Int a
appendArrays arr1 arr2 = listArray (0,n1+n2+1) $ (elems arr1 ++ elems arr2) where
  (0,n1) = bounds arr1
  (0,n2) = bounds arr2

concatArrays :: [Array Int a] -> Array Int a
concatArrays []   = error "concatArrays: empty list"
concatArrays arrs = listArray (0,nn-1) (concatMap elems arrs) where
  sizes = map arraySize arrs
  nn    = sum sizes

interleaveArrays' :: Array Int (Array Int a) -> Array Int a
interleaveArrays' arrs 
  | nubOrd (elems sizes) == [n]   = big
  | otherwise                     = error "interleaveArrays': incompatible array sizes" 
  where
    m     = arraySize arrs
    sizes = fmap arrayLength arrs
    n     = sizes!0
    big   = listArray (0,n*m-1) [ (arrs!j)!i | i<-[0..n-1] , j<-[0..m-1] ]

interleaveArrays :: [Array Int a] -> Array Int a
interleaveArrays arrayList = interleaveArrays' (listToArray arrayList)

-- | This is the inverse of @interleaveArrays@. The integer parameter is the number
-- of output vectors (or \"stride\")
untangleArray :: Int -> Array Int a -> [Array Int a]
untangleArray stride input 
  | r /= 0     = error "untangleArrays: input array's size is not divisible by the stride"
  | otherwise  = pieces
  where
    n = arraySize input
    (q,r) = divMod n stride
    pieces = [ extractCosetArray j stride input | j<-[0..stride-1] ]
{-
    pieces = 
      [ listArray (0,q-1) [ input ! (j + i*stride) | i <- [0..q-1] ] 
      | j <- [0..stride-1] 
      ]
-}

untangleArray' :: Int -> Array Int a -> Array Int (Array Int a)
untangleArray' stride = listToArray . untangleArray stride 

-- | This extracts a subarray with indices of the form @[ offset + i*stride | i<-[0..n-1] ]@
extractCosetArray :: Int -> Int -> Array Int a -> Array Int a
extractCosetArray offset stride input 
  | r /= 0     = error "extractCosetArray: input array's size is not divisible by the stride"
  | otherwise  = piece
  where
    n = arraySize input
    (q,r) = divMod n stride
    piece = listArray (0,q-1) [ input ! (offset + i*stride) | i <- [0..q-1] ] 

-- | This extracts a subarray with indices of the form @[ i*stride | i<-[0..n-1] ]@
extractSubgroupArray :: Int -> Array Int a -> Array Int a
extractSubgroupArray stride = extractCosetArray 0 stride

--------------------------------------------------------------------------------
-- * Binary encoding

instance Binary Log2 where
  put (Log2 k) = putWord8 (fromIntegral k)
  get = (Log2 . fromIntegral) <$> getWord8

putSmallList :: Binary a => [a] -> Put
putSmallList list = do
  let n = length list
  if (n < 256)
    then do 
      putWord8 (fromIntegral n)
      mapM_ put list
    else error "putSmallList: array length >= 256"

getSmallList :: Binary a => Get [a]
getSmallList = do
  len <- fromIntegral <$> getWord8 :: Get Int
  replicateM len get

putSmallArray :: Binary a => Array Int a -> Put
putSmallArray list = do
  let n = arrayLength list
  if (n < 256)
    then do 
      putWord8 (fromIntegral n)
      mapM_ put list
    else error "putSmallArray: array length >= 256"

getSmallArray :: Binary a => Get (Array Int a)
getSmallArray = do
  len <- fromIntegral <$> getWord8 :: Get Int
  listToArray <$> replicateM len get

--------------------------------------------------------------------------------
