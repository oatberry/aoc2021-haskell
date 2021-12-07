{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day5 (day5) where

import Common
import Control.Lens
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Linear.V2
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal)

newtype Diagram a = Diagram (Map (V2 Int) a)
  deriving (Show, Foldable)

instance Num a => Semigroup (Diagram a) where
  (Diagram m1) <> (Diagram m2) = Diagram $ Map.unionWith (+) m1 m2

instance Num a => Monoid (Diagram a) where
  mempty = Diagram Map.empty

type Vent = (V2 Int, V2 Int)

parser :: Parser [Vent]
parser = many $ ventP <* "\n"
  where
    ventP = (,) <$> (pairP <* " -> ") <*> pairP
    pairP = V2 <$> (decimal <* ",") <*> decimal

toDiagram :: Vent -> Diagram Int
toDiagram (V2 x1 y1, V2 x2 y2) = Diagram $
  Map.fromList $ case (compare x1 x2, compare y1 y2) of
    (LT, LT) -> combine [xMin .. xMax] [yMin .. yMax]
    (LT, GT) -> combine [xMin .. xMax] [yMax, yMax - 1 .. yMin]
    (GT, LT) -> combine [xMax, xMax - 1 .. xMin] [yMin .. yMax]
    (GT, GT) -> combine [xMax, xMax - 1 .. xMin] [yMax, yMax - 1 .. yMin]
    _ -> [(V2 a b, 1) | a <- [xMin .. xMax], b <- [yMin .. yMax]]
  where
    combine xs ys = (,1) <$> zipWith V2 xs ys
    (xMin, xMax) = (min x1 x2, max x1 x2)
    (yMin, yMax) = (min y1 y2, max y1 y2)

nonDiagonal :: Eq a => (V2 a, V2 a) -> Bool
nonDiagonal (V2 x1 y1, V2 x2 y2) = x1 == x2 || y1 == y2

solve :: [Vent] -> Int
solve = lengthOf (folded . filtered (> 1)) . foldMap toDiagram

day5 :: Day
day5 = Day 5 parser (solve . filter nonDiagonal) solve
