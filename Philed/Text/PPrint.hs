{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Philed.Text.PPrint (PPrint,write,line,window,render) where

import Control.Monad.State
import Control.Monad.Writer

newtype DiffList a = DiffList { runDiffList :: [a] -> [a] }

unDiffList :: DiffList a -> [a]
unDiffList xs = runDiffList xs []

diffList :: [a] -> DiffList a
diffList xs = DiffList (++ xs)

instance Monoid (DiffList a) where
  mempty        = DiffList id
  mappend xs ys = DiffList (runDiffList ys . runDiffList xs)

newtype PPrint a = PPrint { runPP :: StateT Int (Writer (DiffList Char)) a }
                 deriving (Monad, MonadState Int, MonadWriter (DiffList Char))

write :: String -> PPrint ()
write = tell . diffList

line :: PPrint ()
line = do
  write "\n"
  i <- get
  write (replicate i ' ')

window :: PPrint a -> PPrint a
window s = do
  i <- get
  put (i+2)
  x <- s
  put i
  return x

render :: PPrint () -> String
render s = unDiffList $ execWriter (evalStateT (runPP s) 0)
