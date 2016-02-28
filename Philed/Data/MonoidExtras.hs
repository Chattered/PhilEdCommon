module Philed.Data.MonoidExtras where

import Data.Function
import Data.Monoid
import Philed.Data.NNeg
import Prelude hiding (pred)

-- | Recursive definitions of natural number multiplication
multiplyN :: Monoid m => N -> m -> m
multiplyN Z     _ = mempty
multiplyN (S n) x = x <> multiplyN n x

multiply :: (Integral a, Monoid m) => NNeg a -> m -> m
multiply n = (toN n `multiplyN`)

multiplyInf :: Monoid m => m -> m
multiplyInf x = fix (x <>)

newtype EndoK m a = EndoK { runEndoK :: a -> m a }

instance Monad m => Monoid (EndoK m a) where
  mempty                      = EndoK pure
  mappend (EndoK f) (EndoK g) = EndoK h
    where h x = f x >>= g
