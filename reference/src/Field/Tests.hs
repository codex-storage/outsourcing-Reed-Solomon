
module Field.Tests where

--------------------------------------------------------------------------------

import Control.Monad

import Data.Proxy
import Data.IORef

import Class.Field
import Field.Properties

import Field.Goldilocks           ( F    )
import Field.Goldilocks.Extension ( FExt )

--------------------------------------------------------------------------------

nn = 1000

runMyFieldTests :: IO Bool
runMyFieldTests = do 
  ok1 <- runGoldilocksTests
  ok2 <- runGoldilocksExtensionTests
  return (ok1 && ok2)

--------------------------------------------------------------------------------

runGoldilocksTests :: IO Bool
runGoldilocksTests = do
  putStrLn "\nTests for the Goldilocks field:"
  putStrLn   "==============================="
  okflag <- newIORef True
  runFieldTests okflag nn (Proxy @F)
  readIORef okflag

runGoldilocksExtensionTests :: IO Bool
runGoldilocksExtensionTests = do
  putStrLn "\nTests for the Goldilocks quadratic extension field:"
  putStrLn   "==================================================="
  okflag <- newIORef True
  runFieldTests okflag nn (Proxy @FExt)
  readIORef okflag

--------------------------------------------------------------------------------


