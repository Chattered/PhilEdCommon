module Philed.Control.Monad where

import Control.Monad

zipWithStrictM :: MonadPlus m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithStrictM _ [] []         = return []
zipWithStrictM _ (_:_) []      = mzero
zipWithStrictM _ [] (_:_)      = mzero
zipWithStrictM f (x:xs) (y:ys) = liftM2 (:) (f x y) (zipWithStrictM f xs ys)

zipWithStrictM_ :: MonadPlus m => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithStrictM_ f xs ys = zipWithStrictM f xs ys >> return ()

repeatWhileM :: Monad m => m Bool -> m () -> m ()
repeatWhileM cond x = do
  b <- cond
  if b then x >> repeatWhileM cond x else return ()

repeatUntilM :: Monad m => m Bool -> m () -> m ()
repeatUntilM cond x = do
  b <- cond
  if b then return () else x >> repeatUntilM cond x

accumulateM :: MonadPlus m => (a -> m a) -> a -> m a
accumulateM f x = do
  y <- f x
  return y `mplus` accumulateM f y
