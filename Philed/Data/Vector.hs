module Philed.Data.Vector (Vec, UnitV, Isometry
                          ,fromUnitVector, normalise
                          ,(+.), (-.), (*.), dot, cross, norm, magnitude
                          ,project, projectUnit, projectK, projectUnitK
                          ,rot, rot90
                          ,applyIso, applyIsoU) where

newtype UnitV a = UnitV (a,a)
type Vec a = (a,a)

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

newtype Isometry  a = MatrixIso (a,a,a,a)

rot :: Floating a => a -> Isometry a
rot θ = MatrixIso (cos θ, -(sin θ), sin θ, cos θ)

rot90 :: Num a => Isometry a
rot90 = MatrixIso (0, -1, 1, 0)

applyIso :: Num a => Isometry a -> (a,a) -> (a,a)
applyIso (MatrixIso (a,b,c,d)) (x,y) = (a*x+b*y, c*x+d*y)

applyIsoU :: Num a => Isometry a -> UnitV a -> UnitV a
applyIsoU (MatrixIso (a,b,c,d)) (UnitV (x,y)) = UnitV (a*x+b*y, c*x+d*y)
