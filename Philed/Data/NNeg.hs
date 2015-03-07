module Philed.Data.NNeg (NNeg, SumNNeg(..)
                        ,zero, pred, suc, plus, abs, toNum
                        ,isZero, length, lookup) where

import Data.Foldable
import Data.List (genericDrop)
import Data.Maybe
import Data.Monoid hiding (getSum)
import qualified Prelude as P
import Prelude hiding (abs, length, lookup, pred)

newtype NNeg a = NNeg a deriving (Eq,Ord,Show)

isZero :: Integral a => NNeg a -> Bool
isZero = isNothing . pred

zero :: Num a => NNeg a
zero = NNeg 0

suc :: Integral a => NNeg a -> NNeg a
suc (NNeg n) = NNeg (n+1)

pred :: Integral a => NNeg a -> Maybe (NNeg a)
pred (NNeg 0) = Nothing
pred (NNeg n) = Just $ NNeg (n-1)

plus :: Num a => NNeg a -> NNeg a -> NNeg a
plus (NNeg m) (NNeg n) = NNeg (m + n)

times :: Num a => NNeg a -> NNeg a -> NNeg a
times (NNeg m) (NNeg n) = NNeg (m * n)

abs :: Num a => a -> NNeg a
abs n = NNeg (P.abs n)

toNum :: Num a => NNeg a -> a
toNum (NNeg x) = x

newtype SumNNeg a = SumNNeg { getSum :: NNeg a }

instance Integral a => Monoid (SumNNeg a) where
  mempty                        = SumNNeg (NNeg 0)
  mappend (SumNNeg n) (SumNNeg m) = SumNNeg (n `plus` m)

newtype ProdNNeg a = ProdNNeg { getProd :: NNeg a }

instance Integral a => Monoid (ProdNNeg a) where
  mempty                          = ProdNNeg (NNeg 0)
  mappend (ProdNNeg n) (ProdNNeg m) = ProdNNeg (n `times` m)

length :: (Foldable f, Integral b) => f a -> NNeg b
length = foldl' (\n _ -> suc n) (NNeg 0)

lookup :: Integral b => [a] -> NNeg b -> Maybe a
lookup xs n = listToMaybe $ genericDrop (toNum n) xs
