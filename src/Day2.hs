{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day2 (day2) where

import Common
import Data.Foldable (foldl')
import Text.Megaparsec
import Text.Megaparsec.Char

data Direction = Forward Int | Down Int | Up Int
  deriving (Show, Eq)

parser :: Parser [Direction]
parser = many (direction <* newline)
  where
    direction =
      "forward " *> fmap Forward num
        <|> "down " *> fmap Down num
        <|> "up " *> fmap Up num
    num = read <$> some digitChar

part1 :: [Direction] -> Int
part1 = uncurry (*) . foldl' steer (0, 0)
  where
    steer (x, y) = \case
      Forward n -> (x + n, y)
      Down n -> (x, y + n)
      Up n -> (x, y - n)

part2 :: [Direction] -> Int
part2 = (\(x, y, _) -> x * y) . foldl' steer (0, 0, 0)
  where
    steer (x, y, aim) = \case
      Forward n -> (x + n, y + (aim * n), aim)
      Down n -> (x, y, aim + n)
      Up n -> (x, y, aim - n)

day2 :: Day
day2 = Day 2 parser part1 part2
