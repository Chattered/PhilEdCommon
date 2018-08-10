module Philed.Text.PPrint (PPrint,line,window,render) where

import Data.Char
import Data.Monoid
import Data.String

newtype PPrint = PPrint { runPP :: Int -> String }

instance Semigroup PPrint where
  l <> r = PPrint (\i -> runPP l i ++ runPP r i)

instance Monoid PPrint where
  mempty      = PPrint (const "")

instance IsString PPrint where
  fromString str = PPrint (const str)

line :: PPrint
line = PPrint (\i -> "\n" ++ replicate i ' ')

window :: PPrint -> PPrint
window pp = PPrint (\i -> runPP pp (i + 2))

shrinkWhiteLine :: String -> String
shrinkWhiteLine l | all isSpace l = ""
                  | otherwise     = l

render :: PPrint -> String
render pp = unlines $ map shrinkWhiteLine $ lines (runPP pp 0)
