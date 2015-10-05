module Philed.Control.Monad.Trans.FreeExtras where

import Control.Monad
import Control.Monad.Trans.Free

cataM :: (Traversable f, Monad m) => (f a -> a) -> FreeT f m a -> m a
cataM f = e <=< runFreeT
  where e (Pure x) = return x
        e (Free x) = f <$> traverse (cataM f) x
