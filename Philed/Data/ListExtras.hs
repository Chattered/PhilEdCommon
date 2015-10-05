module Philed.Data.ListExtras where

import Data.List
import qualified Data.Map as M

takes :: [a] -> [(a,[a])]
takes xs = zipWith3 (\l x r -> (x,l ++ r)) (inits xs) xs (drop 1 $ tails xs)

dropToLast :: [a] -> [a]
dropToLast []  = []
dropToLast [x] = [x]
dropToLast (_:xs) = dropToLast xs

zipWithStrict :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithStrict _ [] []         = []
zipWithStrict f (x:xs) (y:ys) = f x y : zipWithStrict f xs ys

expF :: (Bounded b, Enum b, Ord b) => [a] -> [b -> a]
expF xs = fmap ((M.!) . M.fromAscList) . traverse (\y -> [(y,x) | x <- xs]) $ ys
  where ys = [minBound..maxBound]

expFPartial :: Ord b => [a] -> [b] -> [b -> Maybe a]
expFPartial xs =
  fmap (flip M.lookup . M.fromAscList) . traverse (\y -> [(y,x) | x <- xs])
