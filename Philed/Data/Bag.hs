module Philed.Data.Bag (Bag
                       ,empty
                       ,singleton,insert,delete,deleteAll,member
                       ,union,difference,subBagOf
                       ,fromList, toList
                       ,all, filter) where

import Control.Monad
import Philed.Data.Pos
import qualified Data.Map as M
import Prelude hiding (all, filter)

newtype Bag a = Bag (M.Map a (Pos Int))

empty :: Bag a
empty = Bag $ M.empty

insert :: Ord a => a -> Bag a -> Bag a
insert x (Bag m) = Bag (M.insertWith add x one m)

singleton :: a -> Bag a
singleton x = Bag (M.singleton x (Pos 1))

union :: Ord a => Bag a -> Bag a -> Bag a
union (Bag m1) (Bag m2) = Bag (M.unionWith add m1 m2)

delete :: Ord a => a -> Bag a -> Bag a
delete x (Bag m) = Bag (M.update (`sub` one) x m)

deleteAll :: Ord a => a -> Bag a -> Bag a
deleteAll x (Bag m) = Bag (M.delete x m)

difference :: Ord a => Bag a -> Bag a -> Bag a
difference (Bag m1) (Bag m2) = Bag (M.differenceWith sub m1 m2)

member :: Ord a => a -> Bag a -> Bool
member x (Bag m) = M.member x m

subBagOf :: Ord a => Bag a -> Bag a -> Bool
subBagOf (Bag m) (Bag n) = M.isSubmapOfBy (<=) m n

fromList :: Ord a => [a] -> Bag a
fromList xs = foldr insert empty xs

toList :: Ord a => Bag a -> [(a,Int)]
toList (Bag m) = map (\(x,Pos n) -> (x,n)) $ M.toList m

all :: (a -> Bool) -> Bag a -> Bool
all p (Bag m) = M.foldl (&&) True (M.mapWithKey (fmap p . const) m)

filter :: (a -> Bool) -> Bag a -> Bag a
filter p (Bag m) = Bag (M.filterWithKey (fmap p . const) m)
