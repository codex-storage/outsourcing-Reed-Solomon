{-# LANGUAGE CPP #-}

-- #ifdef USE_NAIVE_HASKELL

module NTT.Poly ( module NTT.Poly.Naive ) where
import NTT.Poly.Naive

-- #else
-- 
-- module NTT.Poly ( module NTT.Poly.Flat ) where
-- import NTT.Poly.Flat
-- 
-- #endif
