module Philed.Control.Monad.Stream
       (Stream, run, delay, fix, toDepth, ofList, thenD, diff
       ,fixDiff, fixDiffBy, nubStream, nubStreamBy) where

import Control.Applicative
import Control.Monad ((>=>), liftM, liftM2, MonadPlus, mzero, mplus, join, msum)
import Data.List     hiding (inits,iterate)
import Data.Monoid
import Prelude       hiding (iterate)

transposeM :: Functor f => f [a] -> [f [a]]
transposeM xs = fmap (take 1) xs : transposeM (fmap (drop 1) xs)

newtype Stream a = Stream { run :: [[a]] }
    deriving (Eq, Show)

delay :: Stream a -> Stream a
delay xs = Stream ([] : run xs)

zipM :: MonadPlus m => [m a] -> [m a] -> [m a]
zipM xs []         = xs
zipM [] ys         = ys
zipM (x:xs) (y:ys) = (x `mplus` y) : zipM xs ys

instance Functor Stream where
  f `fmap` Stream xss = Stream $ map (map f) xss

instance Alternative Stream where
  empty = mzero
  (<|>) = mplus

instance Applicative Stream where
  pure      = return
  fs <*> xs = liftM2 ($) fs xs

instance Monad Stream where
  return x = Stream [[x]]
  xs >>= f = (Stream . j . map (map (run .f)) . run) xs
    where j []     = []
          j (x:xs) = zipM (map join (transpose x)) ([] : j xs)

instance MonadPlus Stream where
    mzero = Stream []
    Stream xss `mplus` Stream yss = Stream (zipM xss yss)

fix :: (Stream a -> Stream a) -> Stream a -> Stream a
fix f b = xs where
  xs = b `mplus` delay (f xs)

toDepth :: Int -> Stream a -> Stream a
toDepth n = Stream . take n . run

ofList :: [a] -> Stream a
ofList xs = Stream [xs]

thenD :: Stream a -> Stream a -> Stream a
thenD xs ys = xs `mplus` delay ys

diffBy :: (a -> a -> Bool) -> Stream a -> Stream a -> Stream a
diffBy p xs ys = Stream $ loop [] (run xs) (run ys)
  where loop _ [] _            = []
        loop _ xs []           = xs
        loop acc (x:xs) (y:ys) =
          filter (notElemBy p (y ++ acc)) x : loop (y ++ acc) xs ys

diff :: Eq a => Stream a -> Stream a -> Stream a
diff = diffBy (==)

notElemBy :: (a -> a -> Bool) -> [a] -> a -> Bool
notElemBy p xs x = null (filter (p x) xs)

fixDiffBy :: (a -> a -> Bool) -> (Stream a -> Stream a) -> Stream a -> Stream a
fixDiffBy p f = fix (\xs -> diffBy p (f xs) xs)

fixDiff :: Eq a => (Stream a -> Stream a) -> Stream a -> Stream a
fixDiff = fixDiffBy (==)

nubStreamBy :: (a -> a -> Bool) -> Stream a -> Stream a
nubStreamBy p xs = Stream $ loop [] (run xs)
  where loop _   []     = []
        loop acc (x:xs) =
          let z = filter (notElemBy p acc) (nubBy p x) in
          z : loop (z ++ filter (notElemBy p z) acc) xs

nubStream :: (Eq a) => Stream a -> Stream a
nubStream = nubStreamBy (==)
