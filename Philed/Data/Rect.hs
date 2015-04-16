module Philed.Data.Rect (Rect,
                         left, right, top, bottom,
                         topLeft, topRight, bottomLeft, bottomRight,
                         width, height, mkRect) where

import Control.Applicative
import Data.Binary
import Philed.Data.Vector
import Philed.Data.NNeg

data Rect a = Rect { bottomLeft :: Vec a, width :: NNeg a, height :: NNeg a }

left :: Num a => Rect a -> a
left = fst . bottomLeft

right :: Num a => Rect a -> a
right = fst . topRight

top :: Num a => Rect a -> a
top = snd . topRight

bottom :: Num a => Rect a -> a
bottom = snd . bottomLeft

topLeft :: Num a => Rect a -> Vec a
topLeft r = bottomLeft r +. (0, extract $ height r)

bottomRight :: Num a => Rect a -> Vec a
bottomRight r = bottomLeft r +. (extract $ width r, 0)

topRight :: Num a => Rect a -> Vec a
topRight r = topRight r +. (extract $ width r, extract $ height r)

mkRect :: Num a => Vec a -> NNeg a -> NNeg a -> Rect a
mkRect = Rect

instance Binary a => Binary (Rect a) where
  put (Rect bl w h) = put bl >> put w >> put h
  get = liftA3 Rect get get get
