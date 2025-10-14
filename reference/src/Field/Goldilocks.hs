
{-# LANGUAGE CPP #-}

#ifdef USE_NAIVE_HASKELL

module Field.Goldilocks ( module Field.Goldilocks.Slow ) where
import Field.Goldilocks.Slow

#else

module Field.Goldilocks ( module Field.Goldilocks.Fast ) where
import Field.Goldilocks.Fast

#endif
