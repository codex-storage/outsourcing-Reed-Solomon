
{-# LANGUAGE CPP #-}

#ifdef USE_NAIVE_HASKELL

module Field.Goldilocks.Extension ( module Field.Goldilocks.Extension.Haskell ) where
import Field.Goldilocks.Extension.Haskell

#else

module Field.Goldilocks.Extension ( module Field.Goldilocks.Extension.BindC ) where
import Field.Goldilocks.Extension.BindC

#endif
