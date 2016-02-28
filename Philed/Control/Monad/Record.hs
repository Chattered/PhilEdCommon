{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- A weaker version of State, where updates are restricted to addition for some
-- monoid.

module Philed.Control.Monad.Record (RecordT, runRecordT, evalRecordT, execRecordT
                                   ,MonadRecord, record, get) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Control.Monad.State as S
import Data.Monoid

class Monad m => MonadRecord s m | m -> s where
  record :: s -> m ()
  get    :: m s

newtype RecordT s m a = RecordT { unRecordT :: S.StateT s m a }
                      deriving (Applicative, Functor, Monad)

runRecordT :: (Monoid s, Monad m) => RecordT s m a -> m (a,s)
runRecordT x = S.runStateT (unRecordT x) mempty

evalRecordT :: (Monoid s, Monad m) => RecordT s m a -> m a
evalRecordT x = S.evalStateT (unRecordT x) mempty

execRecordT :: (Monoid s, Monad m) => RecordT s m a -> m s
execRecordT x = S.execStateT (unRecordT x) mempty

instance (Monoid s, Monad m) => MonadRecord s (RecordT s m) where
  record x = RecordT (S.modify $ (<> x))
  get      = RecordT S.get

instance MonadTrans (RecordT s) where
  lift = RecordT . lift

instance (Monad m, MonadError e m) => MonadError e (RecordT s m) where
  throwError     = lift . throwError
  catchError x h = RecordT (catchError (unRecordT x) (unRecordT . h))

instance (Monad m, S.MonadState s m) => S.MonadState s (RecordT t m) where
  get = lift S.get
  put = lift . S.put

instance MonadPlus m => Alternative (RecordT s m) where
  empty = lift mzero
  (<|>) x y = RecordT (unRecordT x <|> unRecordT y)

instance MonadPlus m => MonadPlus (RecordT s m) where
  mzero = empty
  mplus = (<|>)

instance (Monad m, MonadReader r m) => MonadReader r (RecordT e m) where
  ask                 = lift ask
  local f (RecordT s) = RecordT (local f s)

instance (Monad m, MonadWriter w m) => MonadWriter w (RecordT e m) where
  tell               = lift . tell
  listen (RecordT s) = RecordT (listen s)
  pass   (RecordT s) = RecordT (pass s)

instance (Monad m, MonadRecord s m) => MonadRecord s (ExceptT e m) where
  record = lift . record
  get    = lift get

instance MonadIO m => MonadIO (RecordT s m) where
  liftIO = lift . liftIO
