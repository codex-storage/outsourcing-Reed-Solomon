
{-# LANGUAGE CPP #-}

#ifdef USE_NAIVE_HASKELL

module Hash.State ( module Hash.State.Naive ) where
import Hash.State.Naive

#else

module Hash.State ( module Hash.State.FastC ) where
import Hash.State.FastC

#endif
