module Philed.Data.SortedList (SortedList
                              ,extract, fromList, empty, singleton
                              ,elem
                              ,union, intersect, difference) where

import Data.List hiding (elem, intersect, union)
import Prelude hiding (elem)

newtype SortedList a = SortedList [a]

instance Show a => Show (SortedList a) where
  show (SortedList xs) = show xs

extract :: SortedList a -> [a]
extract (SortedList xs) = xs

fromList :: Ord a => [a] -> SortedList a
fromList = SortedList . sort

empty :: SortedList a
empty = SortedList []

singleton :: a -> SortedList a
singleton x = SortedList [x]

elem :: Ord a => a -> SortedList a -> Bool
elem x (SortedList xs) = e xs
  where e [] = False
        e (y:ys) | x < y     = e ys
                 | x == y    = True
                 | otherwise = False

union :: Ord a => SortedList a -> SortedList a -> SortedList a
union (SortedList xs) (SortedList ys) = SortedList (un xs ys)
  where un [] ys = ys
        un xs [] = xs
        un xs@(x:xs') ys@(y:ys') | x < y  = x : un xs' ys
                                 | x == y = x : un xs' ys'
                                 | x > y  = y : un xs  ys'

intersect :: Ord a => SortedList a -> SortedList a -> SortedList a
intersect (SortedList xs) (SortedList ys) = SortedList (inter xs ys)
  where inter [] ys = []
        inter xs [] = xs
        inter xs@(x:xs') ys@(y:ys') | x < y  = inter xs' ys
                                    | x == y = x : inter xs' ys'
                                    | x > y  = inter xs ys'

difference :: Ord a => SortedList a -> SortedList a -> SortedList a
difference (SortedList xs) (SortedList ys) = SortedList (diff xs ys)
  where diff [] ys = []
        diff xs [] = xs
        diff xs@(x:xs') ys@(y:ys') | x < y  = x : diff xs' ys
                                   | x == y = diff xs' ys'
                                   | x > y  = diff xs ys'
