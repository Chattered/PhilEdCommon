module Philed.Control.Monad.Error where

import Control.Monad.Reader
import Control.Monad
import Control.Monad.Error.Class
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup

isDefined :: MonadError e m => m a -> m Bool
isDefined x = liftM (const True) x `catchError` (const $ return False)

orElse :: MonadError e m => m a -> m a -> m a
orElse x y = x `catchError` (const y)

newtype Else e m a = Else { getElse :: m a }

instance MonadError e m => Semigroup (Else e m a) where
  Else x <> Else y = Else $ x `orElse` y

fallbacks :: MonadError e m => m a -> [m a] -> m a
fallbacks x fs = getElse $ sconcat (Else x :| map Else fs)

finally :: MonadError e m => m a -> m b -> m b
finally x y = (x `catchError` (\e -> y >> throwError e)) >> y

unfoldM :: (MonadError e m, Semigroup w) => (a -> m (a,w)) -> a -> m (a,w)
unfoldM f x = f x >>= loop
  where loop (x,w) = (f x >>= \(y,w') -> loop (y, w <> w')) `orElse` return (x,w)

tryM :: MonadError e m => (a -> m a) -> a -> m a
tryM f x = f x `orElse` return x

andTry :: MonadError e m => (a -> m a) -> (a -> m a) -> (a -> m a)
andTry f g x = f x >>= tryM g

newtype Try e m a = Try { runTry :: a -> m a }

instance MonadError e m => Monoid (Try e m a) where
  mempty                  = Try return
  mappend (Try f) (Try g) = Try (f `andTry` g)

spinM :: MonadError e m => (a -> m a) -> a -> m a
spinM f = f `andTry` spinM f

viewError :: MonadError e m => m a -> m (Either e a)
viewError x = liftM Right x `catchError` (return . Left)
