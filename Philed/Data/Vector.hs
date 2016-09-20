module Philed.Data.Vector (Vec, UnitV, Isometry
                          ,xVec, yVec
                          ,fromUnitVector, normalise
                          ,(+.), (-.), (*.), dot, cross, norm, magnitude
                          ,project, projectUnit, projectK, projectUnitK
                          ,rot, rot90, rot180, rot270, reflX, reflY, refl
                          ,applyIso, applyIsoU) where

import Data.Monoid

newtype UnitV a = UnitV (a,a) deriving (Eq, Show)
type Vec a = (a,a)

xVec :: Num a => UnitV a
xVec = UnitV (1,0)

yVec :: Num a => UnitV a
yVec = UnitV (0,1)

fromUnitVector :: UnitV a -> (a,a)
fromUnitVector (UnitV v) = v

(-.) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(ux,uy) -. (vx,vy) = (ux-vx,uy-vy)

(+.) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(ux,uy) +. (vx,vy) = (ux+vx,uy+vy)

(*.) :: Num a => a -> (a, a) -> (a, a)
k *. (x,y) = (k*x,k*y)

(/.) :: Fractional a => (a, a) -> a -> (a, a)
(x,y) /. k = (x/k, y/k)

dot :: Num a => (a, a) -> (a, a) -> a
dot (ux,uy) (vx,vy) = ux*vx+uy*vy

cross :: Num a => (a, a) -> (a, a) -> a
cross (ux,uy) (vx,vy) = ux*vy-uy*vx

norm :: Num a => (a,a) -> a
norm v = v `dot` v

magnitude :: Floating a => (a,a) -> a
magnitude v = sqrt (norm v)

projectK :: Floating a => (a,a) -> (a,a) -> a
projectK u v = ((u `dot` v) / magnitude u)

projectUnitK :: Num a => UnitV a -> (a,a) -> a
projectUnitK (UnitV u) v = u `dot` v

project :: Floating a => (a,a) -> (a,a) -> (a,a)
project u v = projectK u v *. u

projectUnit :: Num a => UnitV a -> (a,a) -> (a,a)
projectUnit u v = projectUnitK u v *. fromUnitVector u

normalise :: Floating a => (a,a) -> UnitV a
normalise v = UnitV (v /. magnitude v)

newtype Isometry a = MatrixIso (a,a,a,a) deriving Show

instance Num a =>  Monoid (Isometry a) where
  mempty = MatrixIso (1,0,0,1)
  mappend (MatrixIso (a,b,c,d)) (MatrixIso (e,f,g,h)) =
    MatrixIso (a*e+b*g, a*f+b*h, c*e+d*g, c*f+d*h)

rot :: Floating a => a -> Isometry a
rot θ = MatrixIso (cos θ, -(sin θ), sin θ, cos θ)

refl :: Floating a => a -> Isometry a
refl θ = MatrixIso (cos (2*θ), (sin (2*θ)), sin (2*θ), -cos (2*θ))

rot90 :: Num a => Isometry a
rot90 = MatrixIso (0,-1,1,0)

rot180 :: Num a => Isometry a
rot180 = rot90 <> rot90

rot270 :: Num a => Isometry a
rot270 = rot180 <> rot90

reflX :: Num a => Isometry a
reflX = MatrixIso (1,0,0,-1)

reflY :: Num a => Isometry a
reflY = MatrixIso (-1,0,0,1)

applyIso :: Num a => Isometry a -> (a,a) -> (a,a)
applyIso (MatrixIso (a,b,c,d)) (x,y) = (a*x+b*y, c*x+d*y)

applyIsoU :: Num a => Isometry a -> UnitV a -> UnitV a
applyIsoU (MatrixIso (a,b,c,d)) (UnitV (x,y)) = UnitV (a*x+b*y, c*x+d*y)
