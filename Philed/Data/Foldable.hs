module Philed.Data.Foldable where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Philed.Data.Monoid
import Prelude hiding (all)

isEmpty :: Foldable f => f a -> Bool
isEmpty = all (const False)

first :: Foldable t => t a -> Maybe a
first = getFirst . foldMap (First . pure)

last :: Foldable t => t a -> Maybe a
last = getLast . foldMap (Last . pure)
