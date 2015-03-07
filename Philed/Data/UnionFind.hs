{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving #-}

module Philed.Data.UnionFind (equiv,union,runUF,UnionFind) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.ST
import Data.Array.ST
import Data.Function

data Cell i = Normal i
            | Rewrite i

newtype UF s i m a = UF { unUF' :: ReaderT (STArray s i (Cell i)) m a }
                   deriving (Functor, Applicative
                            ,MonadReader (STArray s i (Cell i)), Monad, MonadTrans)

normal :: Ix i => i -> UF s i (ST s) i
normal i = do
  uf <- ask
  c  <- lift (readArray uf i)
  case c of
    Rewrite j -> do j' <- normal j
                    lift (writeArray uf i (Rewrite j'))
                    return j'
    Normal j  -> return j

equiv' :: Ix i => i -> i -> UF s i (ST s) Bool
equiv' = liftM2 (==) `on` normal

union' :: Ix i => i -> i -> UF s i (ST s) ()
union' i j = do
  uf <- ask
  i' <- normal i
  j' <- normal j
  if i' == j' then return () else lift (writeArray uf i' (Rewrite j'))

runUF' :: Ix i => (forall s. UF s i (ST s) a) -> (i,i) -> a
runUF' uf bounds = runST (do
  newListArray bounds (map Normal (range bounds)) >>= runReaderT (unUF' uf))

-- Make types look a bit nicer
newtype UnionFind s i a = UnionFind { unUnionFind :: (UF s i (ST s) a) }
                        deriving (Functor, Applicative, Monad)

equiv :: Ix i => i -> i -> UnionFind s i Bool
equiv s t = UnionFind (equiv' s t)

union :: Ix i => i -> i -> UnionFind s i ()
union s t = UnionFind (union' s t)

runUF :: Ix i => (forall s. UnionFind s i a) -> (i,i) -> a
runUF s = runUF' (unUnionFind s)
