{-# LANGUAGE FlexibleContexts #-}
module Philed.Text.Parsec where

import Control.Applicative
import Data.Char
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Semigroup.Traversable
import qualified Philed.Data.NNeg as N
import qualified Philed.Data.Pos as P
import Philed.Data.Semigroup
import Text.Parsec hiding (many, (<|>))

many1ne :: Stream s m t => ParsecT s u m a -> ParsecT s u m (NonEmpty a)
many1ne p = (:|) <$> p <*> (many p)

natsToNat :: Num a => [N.NNeg a] -> N.NNeg a
natsToNat = nsum . zipWith N.times (iterate (N.times (N.abs 10)) (N.abs 1)) . reverse
  where nsum = foldr N.plus N.zero

parseDigit :: (Integral a, Stream s m Char) => ParsecT s u m (N.NNeg a)
parseDigit = fmap (N.abs . fromIntegral . digitToInt) digit

parseNatDigits :: (Integral a, Stream s m Char) => P.P -> ParsecT s u m (N.NNeg a)
parseNatDigits p =
  unwrapMonad . fmap (natsToNat . toList) . sequence1 $ p `multiplyP1` (digit :| [])
  where digit = WrapMonad parseDigit

parseIntDigits :: (Integral a, Stream s m Char) => P.P -> ParsecT s u m a
parseIntDigits p = (char '-' >> negate <$> pi) <|> pi
  where pi = N.extract <$> parseNatDigits p

parseNat :: Integral a => Stream s m Char => ParsecT s u m (N.NNeg a)
parseNat = (natsToNat . toList . digits) <$> many1ne digit
 where digits = fmap (N.abs . fromIntegral . digitToInt)

parseInteger :: Stream s m Char => ParsecT s u m Integer
parseInteger = (char '-' >> negate <$> pi) <|>  pi
 where pi = N.extract <$> parseNat
