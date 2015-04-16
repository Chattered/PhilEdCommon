module Philed.Data.Semigroup where

import Data.Function
import Data.Semigroup
import Philed.Data.Pos
import Prelude hiding (pred)

-- | Recursive definitions of natural number multiplication
multiplyP1 :: Semigroup m => P -> m -> m
multiplyP1 SZ    x = x
multiplyP1 (S p) x = x <> multiplyP1 p x

multiply1 :: (Integral a, Semigroup m) => Pos a -> m -> m
multiply1 p = (toP p `multiplyP1`)

multiplyInf :: Semigroup m => m -> m
multiplyInf x = fix (x <>)
