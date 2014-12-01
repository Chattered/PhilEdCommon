module Philed.Data.Forest (Forest(..), Tree(..), Cursor
                          ,unzipper, view
                          ,top, left, right, up, down, siblings, cousins, level
                          ,filterC, descendents) where

import Data.List (tails)
import Control.Monad

newtype Forest a = Forest { unforest :: [Tree a] }
                 deriving (Eq,Ord,Read,Show)
data Tree a      = Node { node :: a, children :: (Forest a) }
                 deriving (Eq,Ord,Read,Show)

data Cursor a = Cursor { _content :: Tree a
                       , _lefts   :: [Tree a]
                       , _rights  :: [Tree a]
                       , _parents :: [([Tree a], a, [Tree a])]
                       }

unzipper :: Cursor a -> Forest a
unzipper c = Forest (uz (_parents c) (_content c) (_lefts c) (_rights c))
  where uz [] c' ls rs               = ls ++ [c'] ++ rs
        uz ((ls',n,rs'):ps) c' ls rs =
          uz ps (Node n (Forest (ls ++ [c'] ++ rs))) ls' rs'

view :: Cursor a -> Tree a
view = _content

mids :: [a] -> [([a],a,[a])]
mids xs = ms [] xs
  where ms _ []       = []
        ms pre (x:xs) = (pre,x,xs) : ms (x:pre) xs

top :: Forest a -> [Cursor a]
top f = [ Cursor t ls rs [] | (ls,t,rs) <- mids (unforest f) ]

left :: Cursor a -> [Cursor a]
left (Cursor t ls rs ps) = [ Cursor l ls (t:rs) ps
                            | (l,ls) <- zip ls (tails (drop 1 ls)) ]

right :: Cursor a -> [Cursor a]
right (Cursor t ls rs ps) = [ Cursor r (t:ls) rs ps
                            | (r,rs) <- zip rs (tails (drop 1 rs)) ]

up :: Cursor a -> [Cursor a]
up (Cursor t ls rs ps) = [ Cursor (Node x (Forest (reverse ls ++ [t] ++ rs)))
                           ls' rs' ps
                         | (ls',x,rs'):ps <- [ps] ]

down :: Cursor a -> [Cursor a]
down (Cursor (Node x cs) ls rs ps) =
  [ Cursor c ls' rs' ((ls, x, rs) : ps) | (ls',c,rs') <- mids (unforest cs) ]

siblings :: Cursor a -> [Cursor a]
siblings c = left c ++ right c

level :: Cursor a -> [Cursor a]
level c = left c ++ [c] ++ right c

cousins :: Int -> Cursor a -> [Cursor a]
cousins 0 c = siblings c
cousins n c = do
  p     <- up c
  uncle <- cousins (n-1) p
  down uncle

require :: MonadPlus m => (a -> Bool) -> a -> m a
require p x | p x       = return x
            | otherwise = mzero

filterC :: (a -> Bool) -> Cursor a -> [Cursor a]
filterC p = require (p . node . _content)

descendents :: Cursor a -> [Cursor a]
descendents = liftM2 (++) down (down >=> descendents)
