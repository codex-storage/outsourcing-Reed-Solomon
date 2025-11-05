
{-# LANGUAGE CPP #-}

-- #ifdef USE_NAIVE_HASKELL

module NTT.FFT ( module NTT.FFT.Slow ) where
import NTT.FFT.Slow

-- #else
-- 
-- module NTT.FFT ( module NTT.FFT.Fast ) where
-- import NTT.FFT.Fast
-- 
-- #endif
