-- This module is a lie
-- See Pos.hs

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Philed.Data.NNeg (NNeg, N(..)
                        ,isZero, zero, one, suc, pred, predN
                        ,sub, subN, times, timesN, abs, extract
                        ,fromPos, fromNum, fromWord, fromWord8, fromWord16
                        ,fromWord32, fromWord64
                        ,nnIntToInteger
                        ,fromN, toN
                        ,SumNNeg(..), ProdNNeg(..)
                        ,length, lengthN, lookup, lookupN) where

import Control.Monad
import Data.Binary
import Data.Foldable hiding (length)
import Data.Functor.Identity
import Data.List (genericDrop)
import Data.Maybe
import Data.Monoid hiding (getSum)
import Data.Semiring
import qualified Philed.Data.Pos as Pos
import qualified Prelude as P
import Prelude hiding (abs, length, lookup, pred)
import Test.QuickCheck.Arbitrary

newtype NNeg a = NNeg a deriving (Eq, Ord, Show)
data N = Z | S N deriving (Eq, Ord)

isZero :: (Eq a, Num a) => NNeg a -> Bool
isZero (NNeg x) = x == 0

suc :: Num a => NNeg a -> NNeg a
suc (NNeg x) = NNeg $ x + 1

pred :: (Num a, Ord a) => NNeg a -> Maybe (NNeg a)
pred (NNeg x) = guard (x >= 1) >> pure (NNeg $ x - 1)

predN :: N -> Maybe N
predN Z     = Nothing
predN (S n) = Just n

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

fromWord :: Word -> NNeg Word
fromWord = NNeg

fromWord8 :: Word8 -> NNeg Word8
fromWord8 = NNeg

fromWord16 :: Word16 -> NNeg Word16
fromWord16 = NNeg

fromWord32 :: Word32 -> NNeg Word32
fromWord32 = NNeg

fromWord64 :: Word64 -> NNeg Word64
fromWord64 = NNeg

fromNum :: (Num a, Ord a) => a -> Maybe (NNeg a)
fromNum x = if x >= 0 then pure (NNeg x) else mzero

fromPos :: Num a => Pos.Pos a -> NNeg a
fromPos p = NNeg (Pos.extract p)

extract :: Num a => NNeg a -> a
extract (NNeg x) = x

nnIntToInteger :: NNeg Int -> NNeg Integer
nnIntToInteger (NNeg x) = NNeg (fromIntegral x)

toN :: Integral a => NNeg a -> N
toN n = case extract n of
  0 -> Z
  n -> S . toN . NNeg $ n - 1

fromN :: Integral a => N -> NNeg a
fromN Z     = NNeg 0
fromN (S n) = suc (fromN n)

newtype SumNNeg a = SumNNeg { getSum  :: NNeg a }
                  deriving (Eq, Ord, Show)
newtype SumN      = SumN    { getSumN :: N }
                  deriving (Eq, Ord)

instance Num a => Semigroup (NNeg a) where
  NNeg m <> NNeg n = NNeg (m + n)

instance Num a => Monoid (NNeg a) where
  mempty                    = NNeg 0

instance Num a => Semiring (NNeg a) where
  one               = NNeg 1
  NNeg x <.> NNeg y = NNeg (x * y)

instance Semigroup SumN where
  SumN m <> SumN n = SumN (m `plusN` n)

instance Monoid SumN where
  mempty                    = SumN Z

newtype ProdNNeg a = ProdNNeg { getProd  :: NNeg a }
newtype ProdN      = ProdN    { getProdN :: N }

instance Num a => Semigroup (ProdNNeg a) where
  ProdNNeg n <> ProdNNeg m = ProdNNeg (n `times` m)

instance Num a => Monoid (ProdNNeg a) where
  mempty                            = ProdNNeg zero

instance Semigroup ProdN where
  ProdN m <> ProdN n = ProdN (m `timesN` n)

instance Monoid ProdN where
  mempty                      = ProdN Z

lengthN :: Foldable f => f a -> N
lengthN = getSumN . foldMap (const $ SumN (S Z))

length :: (Integral a, Foldable f) => f b -> NNeg a
length = fromN . lengthN

lookupN :: [a] -> N -> Maybe a
lookupN (x:_)  Z     = pure x
lookupN (_:xs) (S n) = xs `lookupN` n
lookupN _ _          = mzero

lookup :: Integral b => [a] -> NNeg b -> Maybe a
lookup xs = lookupN xs . toN

instance Binary a => Binary (NNeg a) where
  put (NNeg x) = put x
  get = NNeg <$> get

instance Arbitrary a => Arbitrary (NNeg a) where
  arbitrary = liftM NNeg arbitrary

instance CoArbitrary a => CoArbitrary (NNeg a) where
  coarbitrary (NNeg n) = coarbitrary n
