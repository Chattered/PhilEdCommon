module Philed.Data.List where

import Data.List

takes :: [a] -> [(a,[a])]
takes xs = zipWith3 (\l x r -> (x,l ++ r)) (inits xs) xs (drop 1 $ tails xs)

dropToLast :: [a] -> [a]
dropToLast []  = []
dropToLast [x] = [x]
dropToLast (_:xs) = dropToLast xs
