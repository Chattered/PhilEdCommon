module Philed.Data.Pos (Pos, P(..)
                       ,isOne, one, suc, pred, predP
                       ,plus, plusP, sub, subP, times, timesP, fromNum, extract
                       ,fromP, toP
                       ,SumPos(..), ProdPos(..)
                       ,length, lengthP) where

import Control.Monad
import Data.Semigroup
import Data.Semigroup.Foldable
import Prelude hiding (length, pred)

newtype Pos a = Pos a deriving (Eq,Ord,Show)
data P = SZ | S P

isOne :: (Eq a, Num a) => Pos a -> Bool
isOne (Pos x) = x == 1

one :: Num a => Pos a
one = Pos 1

suc :: Num a => Pos a -> Pos a
suc (Pos x) = Pos (x + 1)

pred :: (Num a, Ord a) => Pos a -> Maybe (Pos a)
pred (Pos x) = guard (x <= 1) >> (pure . Pos $ x - 1)

predP :: P -> Maybe P
predP SZ    = Nothing
predP (S n) = Just n

plus :: Num a => Pos a -> Pos a -> Pos a
plus (Pos m) (Pos n) = Pos (m + n)

plusP :: P -> P -> P
plusP m SZ    = S m
plusP m (S n) = S (m `plusP` n)

sub :: (Num a, Ord a) => Pos a -> Pos a -> Maybe (Pos a)
sub (Pos x) (Pos y) = if x > y then pure (Pos (x - y)) else mzero

subP :: P -> P -> Maybe P
subP SZ     _    = mzero
subP (S m) SZ    = pure m
subP (S m) (S n) = subP m n

times :: Num a => Pos a -> Pos a -> Pos a
times (Pos m) (Pos n) = Pos (m * n)

timesP :: P -> P -> P
timesP m SZ    = m
timesP m (S n) = (m `timesP` n) `plusP` m

fromNum :: (Ord a, Num a) => a -> Maybe (Pos a)
fromNum n = if n > 0 then pure (Pos n) else mzero

extract :: Num a => Pos a -> a
extract (Pos x) = x

toP :: Integral a => Pos a -> P
toP (Pos 1) = SZ
toP (Pos n) = S . toP . Pos $ n - 1

fromP :: Integral a => P -> Pos a
fromP SZ    = Pos 1
fromP (S n) = suc (fromP n)

newtype SumPos a = SumPos { getSum  :: Pos a }
newtype SumP     = SumP   { getSumP :: P }

instance Num a => Semigroup (SumPos a) where
  (<>) (SumPos m) (SumPos n) = SumPos (m `plus` n)

instance Semigroup SumP where
  (<>) (SumP m) (SumP n) = SumP (m `plusP` n)

newtype ProdPos a = ProdPos { getProd  :: Pos a }
newtype ProdN     = ProdN   { getProdN :: P }

instance Num a => Monoid (ProdPos a) where
  mempty                          = ProdPos one
  mappend (ProdPos n) (ProdPos m) = ProdPos (n `times` m)

instance Monoid ProdN where
  mempty                      = ProdN SZ
  mappend (ProdN m) (ProdN n) = ProdN (m `timesP` n)

lengthP :: Foldable1 f => f a -> P
lengthP = getSumP . foldMap1 (const $ SumP SZ)

length :: (Integral a, Foldable1 f) => f a -> Pos a
length = fromP . lengthP
