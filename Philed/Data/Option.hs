{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Wrapper of Maybe to be an instance of MonadError.

module Philed.Data.Option where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Fix
import Data.Traversable

newtype Option a = Option { runOption :: Maybe a }
                 deriving (Eq, Monad, Functor, Ord, Read, Show, MonadFix
                          ,Alternative, Applicative, Foldable, Traversable
                          ,Semigroup, Monoid, MonadPlus)

instance MonadError () Option where
  throwError () = Option Nothing
  catchError (Option Nothing) f = f ()
  catchError x _                = x
