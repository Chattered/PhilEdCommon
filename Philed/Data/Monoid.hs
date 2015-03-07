{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveTraversable #-}
module Philed.Data.Monoid where

import Data.Function
import Data.Monoid
import Philed.Data.NNeg
import Prelude hiding (pred)

-- | Recursive definition of natural number multiplication
multiply :: (Integral a, Monoid m) => NNeg a -> m -> m
multiply n x = case pred n of
                Nothing -> mempty
                Just p  -> x `mappend` multiply p x

multiplyInf :: Monoid m => m -> m
multiplyInf x = fix (x `mappend`)
