module Philed.Control.Monad.Error where

import Control.Monad
import Control.Monad.Error.Class
import Data.Semigroup

isDefined :: MonadError e m => m a -> m Bool
isDefined x = liftM (const True) x `catchError` (const $ return False)

orElse :: MonadError e m => m a -> m a -> m a
orElse x y = x `catchError` (const y)

finally :: MonadError e m => m a -> m b -> m b
finally x y = (x `catchError` (\e -> y >> throwError e)) >> y

unfoldM :: (MonadError e m, Semigroup w) => (a -> m (a,w)) -> a -> m w
unfoldM f x = f x >>= (\(y,w) -> liftM (w <>) (unfoldM f y) `orElse` return w)

-- | Spin until failure.
loopM :: MonadError e m => (a -> m a) -> a -> m a
loopM f = liftM getLast . unfoldM (liftM (\x -> (x, Last x)) . f)
