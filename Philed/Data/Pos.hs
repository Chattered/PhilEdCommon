module Philed.Data.Pos where

import Control.Monad

newtype Pos a = Pos a deriving (Eq,Ord,Show)

one :: Integral a => Pos a
one = Pos 1

add :: Integral a => Pos a -> Pos a -> Pos a
add (Pos m) (Pos n) = Pos (m + n)

fromInt :: Integral a => a -> Maybe (Pos a)
fromInt n = guard (n > 0) >> return (Pos n)

sub :: Integral a => Pos a -> Pos a -> Maybe (Pos a)
sub (Pos m) (Pos n) = fromInt (m - n)
