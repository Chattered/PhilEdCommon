{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | "Betweenness" is a three-place relation which can be used to define intervals of
-- a space, without requiring there be any linear ordering of points in that
-- space. Totally ordered spaces give rise to such a relation (see V1), as do
-- two-dimensional vectors, interpreted as angles (see V2).

module Philed.Data.Geom.Between (Ordered, V1(..), V2, vec2, getVec2,
                                 getSegments, singleton, isOrdered) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Monoid
import Data.Ratio
import Philed.Data.Vector
import Prelude hiding (abs)
import Test.QuickCheck ((==>), Arbitrary, Property)

class Between a where
  -- | `between a (x,y)` returns true if `a` is equal to either of `x` or `y` or lies
  -- between them.
  between :: a -> (a,a) -> Bool

-- | One-dimensional vector type.
newtype V1 a = V1 a deriving (Arbitrary, Eq, Show)

instance Ord a => Between (V1 a) where
  between (V1 x) (V1 a,V1 b) = a <= x && x <= b || b <= x && x <= a

data Ray a = Ray { rayStart :: Vec a, rayDir :: Vec a } deriving (Eq, Show)

onRay :: (Ord a, Num a) => Vec a -> Ray a -> Bool
onRay x (Ray start p) =
  between (V1 x) (V1 start, V1 p) || between (V1 p) (V1 start, V1 x)

intersectSegRay :: (Num a, Ord a) => (Vec a, Vec a) -> Ray a -> Bool
intersectSegRay (a,b) ray@(Ray rayStart rayDir) =
  let p = rayStart +. rayDir
      r = rayDir
      s = b -. a in
  let rs = r `cross` s in
  let qp = a -. rayStart in
  let x  = qp `cross` r in
  case (rs == 0, x == 0) of
    (True, True) -> onRay a ray-- || onRay b ray
                    -- || (a /= b && between (V1 a) (V1 rayStart, V1 b))
    (True, False) -> False
    (False,_)     -> let t = qp `cross` s
                         u = qp `cross` r
                     in 0 < t * rs && 0 <= u && u <= rs

-- | Non-zero vectors
newtype V2 a = V2 { getVec2 :: Vec a } deriving (Arbitrary, Eq, Show)

vec2 :: (Num a, Eq a) => (a, a) -> Maybe (V2 a)
vec2 (x,y) | x == 0 && y == 0 = mzero
vec2 (x,y) = pure (V2 (x,y))

-- | One non-zero vector lies between two others if it lies in the angle formed by
-- their rays.
instance (Num a, Ord a) => Between (V2 a) where
  between (V2 (x,y)) (V2 (ax,ay), V2 (bx,by)) =
    intersectSegRay ((ax,ay),(bx,by)) (Ray (0,0) (x,y))

-- | A compact representation of a set of segments over a space as defined by
-- betweeness.
newtype Ordered a = Ordered { getSegments :: [(a,a)] } deriving Show

-- | The singleton set of segments over a space.
singleton :: (a,a) -> Ordered a
singleton = Ordered . pure

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

-- Pairs of distinct points form arrows, which may or may not be in the same
-- direction.
sameDir :: (Eq a, Between a) => (a,a) -> (a,a) -> Bool
sameDir (a,b) (c,d) =
  if a == c then between d (a,b) || between b (a,d)
  else if a == d then between a (b,c)
       else if b == c then between b (a,d)
            else if b == d then not (between b (a,c))
                 else between d (c,a)    && between a (c,b)
                      || between a (c,d) && between d (a,b)
                      || between a (c,b) && between b (a,d)
                      || between c (a,d) && between c (a,b)
                      || between c (a,b) && between b (a,d)
                      || between b (a,c) && between c (a,d)

extractDir :: Eq a => Ordered a -> Maybe (a,a)
extractDir (Ordered [])      = Nothing
extractDir (Ordered [(a,b)]) = if a == b then Nothing else Just (a,b)
extractDir (Ordered ((a,_):(_,b):_)) = Just (a,b)

instance (Eq a, Between a) => Monoid (Ordered a) where
  mempty = Ordered []
  mappend xs ys =
    Ordered (case (extractDir xs, extractDir ys) of
               (Nothing, Nothing) -> nub (getSegments xs ++ getSegments ys)
               (Nothing, Just dir) -> union dir (getSegments xs) (getSegments ys)
               (Just dir, Nothing) -> union dir (getSegments xs) (getSegments ys)
               (Just dir, Just dir2) ->
                 if sameDir dir dir2 then
                   union dir (getSegments xs) (getSegments ys)
                 else union dir (getSegments xs) (reverse (swap <$> getSegments ys)))
    where union _ [] cds = cds
          union _ abs [] = abs
          union dir abs@((a,b):abs') cds@((c,d):cds') =
            if a == b then
              if between a (c,d) then
                un abs' ((c,d):cds')
              else if c == d then
                     if sameDir dir (a,c) then (a,b) : un abs' cds
                     else (c,d) : un abs cds'
                   else if between c (a,d) then
                          (a,b) : un abs' cds
                        else (c,d) : un abs cds'
            else if c == d then un cds abs
                 else if b == c then un abs' ((a,d) : cds')
                      else if b == d then
                             if a == c then (a,b) : un abs' cds'
                             else if between a (c,d) then (c,d) : un abs' cds'
                                  else (a,d) : un abs' cds'
                           else if a == c then
                                  if between d (a,b) then un abs cds'
                                  else (a,d) : un abs' cds'
                                else if a == d then un ((c,b) : abs') cds'
                                     else alldistinct
            where alldistinct =
                    if between d (a,c) then (c,d) : un cds' abs
                    else if between a (c,d) && between d (a,b) then
                           un ((c,b) : abs') cds'
                         else if between b (c,d) then un abs' ((a,d):cds)
                              else if between d (a,b) then un abs cds'
                                   else if between b (c,d) then
                                          un abs ((a,d) : cds')
                                        else (a,b) : un abs' cds
                  un = union dir

inOrdered :: Between a => Ordered a -> a -> Bool
inOrdered (Ordered xs) x = or (map (between x) xs)

-------------------------------------------------------------------------------------

isOrdered :: (Eq a, Between a) => Ordered a -> Bool
isOrdered (Ordered []) = True
isOrdered (Ordered [xy]) = True
isOrdered (Ordered ((x,y):(z,w):xys)) =
  y /= z && between y (x,z) && between z (x,w)

type Eight = (Bool,Bool,Bool)

eightList :: Ordered (V1 Eight) -> [V1 Eight]
eightList xs = filter (xs `inOrdered`) eights

eights = map V1
         [(False,False,False)
          ,(False,False,True)
          ,(False,True,False)
          ,(False,True,True)
          ,(True,False,False)
          ,(True,False,True)
          ,(True,True,False)
          ,(True,True,True)
          ]

prop1 :: V1 Eight -> V1 Eight -> V1 Eight -> V1 Eight -> Bool
prop1 a b c d = isOrdered (singleton (a,b) <> singleton (c,d))

prop2 :: V1 Eight -> V1 Eight -> V1 Eight -> V1 Eight -> V1 Eight
         -> V1 Eight -> V1 Eight -> V1 Eight -> Bool
prop2 a b c d e f g h =
  isOrdered (singleton (a,b)
             <> singleton (c,d)
             <> singleton (e,f)
             <> singleton (g,h))

prop3 :: V1 Eight -> V1 Eight -> V1 Eight -> V1 Eight -> Bool
prop3 a b c d = eightList (ab <> cd)
                == filter (liftA2 (||) (inOrdered ab) (inOrdered cd)) eights
  where ab = singleton (a,b)
        cd = singleton (c,d)

prop4 :: V1 Eight -> V1 Eight -> V1 Eight -> V1 Eight -> V1 Eight
         -> V1 Eight -> V1 Eight -> V1 Eight -> Bool
prop4 a b c d e f g h =
  eightList (ab <> cd <> ef <> gh)
  == filter (foldl (liftA2 (||)) (const False)
             . map inOrdered $ [ab,cd,ef,gh]) eights
  where ab = singleton (a,b)
        cd = singleton (c,d)
        ef = singleton (e,f)
        gh = singleton (g,h)
