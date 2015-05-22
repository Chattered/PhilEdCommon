module Philed.Data.SortedListMap (SortedListMap
                                 ,extract, fromList, empty, singleton
                                 ,find, member
                                 ,unionWith, intersectWith, difference, delete) where

import Data.Function
import Data.List hiding (delete, find, intersect, union)
import Data.Maybe

newtype SortedListMap k v = SortedListMap [(k,v)]

instance (Show k, Show v) => Show (SortedListMap k v) where
  show (SortedListMap kvs) = show kvs

extract :: SortedListMap k v -> [(k,v)]
extract (SortedListMap kvs) = kvs

fromList :: Ord k => [(k,v)] -> SortedListMap k v
fromList = SortedListMap . sortBy (compare `on` fst)

empty :: SortedListMap k v
empty = SortedListMap []

singleton :: k -> v -> SortedListMap k v
singleton k v = SortedListMap [(k,v)]

find :: Ord k => k -> SortedListMap k v -> Maybe v
find k (SortedListMap kvs) = e kvs
  where e [] = Nothing
        e ((l,v):lvs) | k < l     = e lvs
                      | k == l    = Just v
                      | otherwise = Nothing

member :: Ord k => k -> SortedListMap k v -> Bool
member k = isJust . find k

unionWith :: Ord k =>
             (v -> v -> v)
             -> SortedListMap k v -> SortedListMap k v -> SortedListMap k v
unionWith f (SortedListMap kvs) (SortedListMap lvs) = SortedListMap (un kvs lvs)
  where un [] lvs = lvs
        un kvs [] = kvs
        un kvs@((k,v):kvs') lvs@((l,v'):lvs') | k < l  = (k,v)      : un kvs' lvs
                                              | k == l = (k,f v v') : un kvs' lvs'
                                              | k > l  = (l,v')     : un kvs  lvs'

intersectWith :: Ord k =>
                 (v -> v -> v)
                 -> SortedListMap k v -> SortedListMap k v -> SortedListMap k v
intersectWith f (SortedListMap kvs) (SortedListMap lvs)
  = SortedListMap (inter kvs lvs)
  where inter [] lvs = []
        inter kvs [] = kvs
        inter kvs@((k,v):kvs') lvs@((l,v'):lvs')
          | k < l  = inter kvs' lvs
          | k == l = (k,f v v') : inter kvs' lvs'
          | k > l  = inter kvs lvs'

difference :: Ord k => SortedListMap k v -> SortedListMap k v -> SortedListMap k v
difference (SortedListMap kvs) (SortedListMap lvs) = SortedListMap (diff kvs lvs)
  where diff [] lvs = []
        diff kvs [] = kvs
        diff kvs@((k,v):kvs') lvs@((l,v'):lvs') | k < l  = (k,v) : diff kvs' lvs
                                                | k == l = diff kvs' lvs'
                                                | k > l  = diff kvs lvs'

delete :: Ord k => k -> SortedListMap k v -> SortedListMap k v
delete k (SortedListMap lvs) = SortedListMap (del lvs)
  where del []                        = []
        del lvs@((l,v):lvs') | k < l  = lvs
                             | k == l = lvs'
                             | k > l  = (l,v) : del lvs'
