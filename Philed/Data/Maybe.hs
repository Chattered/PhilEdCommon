module Philed.Data.Maybe (module Data.Maybe, plusMaybe) where

import Data.Maybe

plusMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
plusMaybe f (Just x) (Just y) = Just (f x y)
plusMaybe _ Nothing y = y
plusMaybe _ x Nothing = x
