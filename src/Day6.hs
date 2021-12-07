{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day6 (day6) where

import Common
import Data.IntMultiSet (IntMultiSet)
import qualified Data.IntMultiSet as MS
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal)

parser :: Parser IntMultiSet
parser = MS.fromList <$> decimal `sepBy` ","

simulate :: Int -> IntMultiSet -> Int
simulate times = MS.size . (!! times) . iterate step
  where
    step = MS.concatMap \case 0 -> [6, 8]; n -> [n - 1]

day6 :: Day
day6 = Day 6 parser (simulate 80) (simulate 256)
