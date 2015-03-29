{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Philed.Data.NNeg (NNeg, N(..)
                        ,isZero, zero, suc, pred, predN
                        ,plus, plusN, sub, subN, times, timesN, abs, extract, fromNum
                        ,fromN, toN
                        ,SumNNeg(..), ProdNNeg(..)
                        ,length, lengthN, lookup, lookupN) where

import Control.Monad
import Data.Foldable hiding (length)
import Data.Functor.Identity
import Data.List (genericDrop)
import Data.Maybe
import Data.Monoid hiding (getSum)
import qualified Prelude as P
import Prelude hiding (abs, length, lookup, pred)

newtype NNeg a = NNeg a deriving (Eq, Ord, Show)
data N = Z | S N

isZero :: (Eq a, Num a) => NNeg a -> Bool
isZero (NNeg x) = x == 0

zero :: Num a => NNeg a
zero = NNeg 0

suc :: Num a => NNeg a -> NNeg a
suc (NNeg x) = NNeg $ x + 1

pred :: (Num a, Ord a) => NNeg a -> Maybe (NNeg a)
pred (NNeg x) = guard (x >= 1) >> pure (NNeg $ x - 1)

predN :: N -> Maybe N
predN Z     = Nothing
predN (S n) = Just n

plus :: Num a => NNeg a -> NNeg a -> NNeg a
plus (NNeg x) (NNeg y) = NNeg $ x + y

plusN :: N -> N -> N
plusN m Z     = m
plusN m (S n) = S (m `plusN` n)

sub :: (Num a, Ord a) => NNeg a -> NNeg a -> Maybe (NNeg a)
sub (NNeg x) (NNeg y) = if x >= y then pure (NNeg (x - y)) else mzero

subN :: N -> N -> Maybe N
subN Z _         = mzero
subN m Z         = pure m
subN (S m) (S n) = subN m n

times :: Num a => NNeg a -> NNeg a -> NNeg a
times (NNeg x) (NNeg y) = NNeg $ x * y

timesN :: N -> N -> N
timesN m Z     = Z
timesN m (S n) = (m `timesN` n) `plusN` m

abs :: Num a => a -> NNeg a
abs x = NNeg . P.abs $ x

fromNum :: (Num a, Ord a) => a -> Maybe (NNeg a)
fromNum x = if x >= 0 then pure (NNeg x) else mzero

extract :: Num a => NNeg a -> a
extract (NNeg x) = x

toN :: Integral a => NNeg a -> N
toN n = case extract n of
  0 -> Z
  n -> S . toN . NNeg $ n - 1

fromN :: Integral a => N -> NNeg a
fromN Z     = NNeg 0
fromN (S n) = suc (fromN n)

newtype SumNNeg a = SumNNeg { getSum  :: NNeg a }
newtype SumN      = SumN    { getSumN :: N }

instance Num a => Monoid (SumNNeg a) where
  mempty                          = SumNNeg zero
  mappend (SumNNeg n) (SumNNeg m) = SumNNeg (n `plus` m)

instance Monoid SumN where
  mempty                    = SumN Z
  mappend (SumN m) (SumN n) = SumN (m `plusN` n)

newtype ProdNNeg a = ProdNNeg { getProd  :: NNeg a }
newtype ProdN      = ProdN    { getProdN :: N }

instance Num a => Monoid (ProdNNeg a) where
  mempty                            = ProdNNeg zero
  mappend (ProdNNeg n) (ProdNNeg m) = ProdNNeg (n `times` m)

instance Monoid ProdN where
  mempty                      = ProdN Z
  mappend (ProdN m) (ProdN n) = ProdN (m `timesN` n)

lengthN :: Foldable f => f a -> N
lengthN = getSumN . foldMap (const $ SumN (S Z))

length :: (Integral a, Foldable f) => f a -> NNeg a
length = fromN . lengthN

lookupN :: [a] -> N -> Maybe a
lookupN (x:_)  Z     = pure x
lookupN (_:xs) (S n) = xs `lookupN` n
lookupN _ _          = mzero

lookup :: Integral b => [a] -> NNeg b -> Maybe a
lookup xs = lookupN xs . toN
