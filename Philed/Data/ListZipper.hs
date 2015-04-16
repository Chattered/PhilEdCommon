{-# LANGUAGE DeriveFunctor #-}

module Philed.Data.ListZipper (ZipperT, Zipper
                              ,leftsT, rightsT, fromNonEmptyT, toNonEmptyT
                              ,fromNonEmpty,  toNonEmpty) where

import Control.Applicative
import Control.Monad
import Control.Comonad
import Control.Comonad.Hoist.Class
import Control.Comonad.Trans.Class
import Data.Foldable (toList)
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import Data.Semigroup
import qualified Data.List.NonEmpty as NE

data ZipperT w a = ZipperT [w a] (w a) [w a] deriving (Functor, Show)

sequenceW :: ComonadApply w => NonEmpty (w a) -> w (NonEmpty a)
sequenceW (x :| [])     = liftW (:| []) x
sequenceW (x :| (y:ys)) = liftW2 (<|) x (sequenceW (y :| ys))

unZipperT :: ComonadApply w => ZipperT w a -> w (NonEmpty a)
unZipperT (ZipperT lws wx rws) = sequenceW (lws `append` (wx :| rws))
  where append :: [a] -> NonEmpty a -> NonEmpty a
        append [] ys            = ys
        append (x:xs) (y :| ys) = x :| (xs ++ [y] ++ ys)

instance ComonadApply w => ComonadApply (ZipperT w) where
  ZipperT lfs f rfs <@> ZipperT lxs x rxs =
    ZipperT (zipWith (<@>) lfs lxs) (f <@> x) (zipWith (<@>) rfs rxs)

instance Applicative w => Applicative (ZipperT w) where
  pure x = ZipperT [] (pure x) []
  ZipperT lfs f rfs <*> ZipperT lxs x rxs =
    ZipperT (zipWith (<*>) lfs lxs) (f <*> x) (zipWith (<*>) rfs rxs)

instance ComonadApply w => Comonad (ZipperT w) where
  extract (ZipperT _ w _) = extract w
  duplicate c             = ZipperT (f <$> leftsT c) (f c) (f <$> rightsT c)
    where
      f (ZipperT [] wx []) = fmap (\wx -> ZipperT [] wx []) (duplicate wx)
      f (ZipperT [] wx (rw:rws)) =
        liftW2 (ZipperT []) (duplicate wx)
                            (NE.toList <$> (sequenceW $ duplicate <$> rw :| rws))
      f (ZipperT (lw:lws) wx []) =
        liftW2 (\lws wx -> ZipperT lws wx [])
               (NE.toList <$> (sequenceW $ duplicate <$> lw :| lws))
               (duplicate wx)
      f (ZipperT (lw:lws) wx (rw:rws)) =
        liftW3 ZipperT (NE.toList <$> (sequenceW $ duplicate <$> lw :| lws))
                       (duplicate wx)
                       (NE.toList <$> (sequenceW $ duplicate <$> rw :| rws))

instance ComonadTrans ZipperT where
  lower (ZipperT _ w _) = w

instance ComonadHoist ZipperT where
  cohoist n (ZipperT lws wx rws) = ZipperT (map n lws) (n wx) (map n rws)

leftsT :: ZipperT w a -> [ZipperT w a]
leftsT (ZipperT [] _ _)          = []
leftsT (ZipperT (lw:lws) wx rws) =
  let c = ZipperT lws lw (wx:rws) in c : leftsT c

rightsT :: ZipperT w a -> [ZipperT w a]
rightsT (ZipperT _ _ [])         = []
rightsT (ZipperT lws wx (rw:rws)) =
  let c = ZipperT (wx:lws) rw rws in c : rightsT c

fromNonEmptyT :: NE.NonEmpty (w a)-> ZipperT w a
fromNonEmptyT (x :| xs) = ZipperT [] x xs

toNonEmptyT :: ZipperT w a -> NE.NonEmpty (w a)
toNonEmptyT (ZipperT ls x rs) = foldl (flip (<|)) (x :| rs) ls

type Zipper = ZipperT Identity

fromNonEmpty :: NE.NonEmpty a-> Zipper a
fromNonEmpty = fromNonEmptyT . fmap Identity

toNonEmpty :: Zipper a -> NE.NonEmpty a
toNonEmpty = fmap runIdentity . toNonEmptyT
