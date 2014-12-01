module Philed.Control.Monad where

import Control.Monad

zipWithStrictM :: MonadPlus m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithStrictM _ [] []         = return []
zipWithStrictM _ (_:_) []      = mzero
zipWithStrictM _ [] (_:_)      = mzero
zipWithStrictM f (x:xs) (y:ys) = liftM2 (:) (f x y) (zipWithStrictM f xs ys)

zipWithStrictM_ :: MonadPlus m => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithStrictM_ f xs ys = zipWithStrictM f xs ys >> return ()
