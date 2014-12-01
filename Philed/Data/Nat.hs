module Philed.Data.Nat (Nat, zero, suc, add, abs) where

import Control.Monad
import qualified Prelude as P
import Prelude hiding (abs)

newtype Nat a = Nat a deriving (Eq,Ord,Show)

zero :: Integral a => Nat a
zero = Nat 0

suc :: Integral a => Nat a -> Nat a
suc (Nat n) = Nat (n+1)

add :: Integral a => Nat a -> Nat a -> Nat a
add (Nat m) (Nat n) = Nat (m + n)

abs :: Integral a => a -> Nat a
abs n = Nat (P.abs n)
