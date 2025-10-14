
-- | Monadic interface to do Fiat-Shamir challenges

{-# LANGUAGE StrictData, GeneralizedNewtypeDeriving #-}
module Hash.Duplex.Monad where

--------------------------------------------------------------------------------

import Data.Array

import Control.Monad
import Control.Monad.Identity
import qualified Control.Monad.State.Strict as S
import Control.Monad.IO.Class

import Text.Show.Pretty

import Field.Goldilocks

import Hash.Common
import Hash.State
import Hash.Duplex.Pure ( DuplexState, Squeeze, Absorb , theHashFunction )
import qualified Hash.Duplex.Pure as Pure

--------------------------------------------------------------------------------
-- * Monadic interface

newtype DuplexT m a 
  = DuplexT (S.StateT DuplexState m a)
  deriving (Functor,Applicative,Monad)

type Duplex a = DuplexT Identity a

runDuplexT :: Monad m => DuplexT m a -> State -> m a
runDuplexT (DuplexT action) ini = S.evalStateT action (Pure.duplexInitialState ini)

runDuplex :: Duplex a -> State -> a
runDuplex action ini = runIdentity (runDuplexT action ini)

absorb :: (Monad m, Absorb a) => a -> DuplexT m ()
absorb x = DuplexT $ S.modify (Pure.absorb x)

squeeze :: (Monad m, Squeeze a) => DuplexT m a
squeeze = DuplexT $ S.state Pure.squeeze

squeezeN :: (Monad m, Squeeze a) => Int -> DuplexT m [a]
squeezeN n = DuplexT $ S.state (Pure.squeezeN n)

-- | For debugging only
inspectDuplexState :: Monad m => DuplexT m (DuplexState) 
inspectDuplexState = DuplexT S.get

--------------------------------------------------------------------------------
-- * Access to the internal state (so that we can implement grinding)

unsafeGetInnerState :: Monad m => DuplexT m DuplexState
unsafeGetInnerState = DuplexT S.get

unsafeSetInnerState :: Monad m => DuplexState -> DuplexT m ()
unsafeSetInnerState s = DuplexT (S.put s)

--------------------------------------------------------------------------------
-- * Duplex in IO

type DuplexIO a = DuplexT IO a

instance MonadIO (DuplexT IO) where 
  liftIO action = DuplexT (liftIO action)

ioToDuplexIO :: IO a -> DuplexIO a
ioToDuplexIO = liftIO

duplexPutStrLn :: String -> DuplexIO ()
duplexPutStrLn s = DuplexT (liftIO $ putStrLn s)

duplexPrint_ :: Show a => a -> DuplexIO ()
duplexPrint_ x = DuplexT (liftIO $ print x)

duplexPrint :: Show a => String -> a -> DuplexIO ()
duplexPrint n x = DuplexT (liftIO $ putStrLn $ n ++ " = " ++ show x)

duplexPPrint :: Show a => String -> a -> DuplexIO ()
duplexPPrint n x = DuplexT (liftIO $ putStrLn $ n ++ ":\n\n" ++ ppShow x ++ "\n")

printDuplexState :: DuplexIO ()
printDuplexState = duplexPrint "state" =<< inspectDuplexState

runDuplexIO :: DuplexIO a -> State -> IO a
runDuplexIO = runDuplexT

runDuplexIO_ :: DuplexIO a -> IO a
runDuplexIO_ action 
  = runDuplexIO action 
  $ zeroState theHashFunction

--------------------------------------------------------------------------------

duplexTest :: Int -> IO ()
duplexTest m = runDuplexT action (zeroState theHashFunction) where

  action :: DuplexIO ()
  action = do
    forM_ [0..19] $ \(k :: Int) -> do
      absorb (map intToF [1..k])
      ys <- squeezeN k :: DuplexIO [F]
      duplexPrint_ ys

--------------------------------------------------------------------------------
