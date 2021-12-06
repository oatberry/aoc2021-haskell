module Day3 (day3) where

import Common
import Data.Foldable (foldl')
import Data.Function (on)
import Data.List (transpose)
import Foreign.Marshal.Utils (fromBool)

parser :: String -> [[Bool]]
parser = map (map (== '1')) . lines

data Superlative = Most | Least

findCommon :: Superlative -> [Bool] -> Bool
findCommon which bits = case which of
  Most -> ones >= zeroes
  Least -> zeroes > ones
  where
    ones = length $ filter id bits
    zeroes = length $ filter not bits

fromBinary :: [Bool] -> Int
fromBinary = foldl' (\acc i -> (acc * 2) + fromBool i) 0

part1 :: [[Bool]] -> Int
part1 input = ((*) `on` fromBinary) gamma (not <$> gamma)
  where
    gamma = findCommon Most <$> transpose input

whittle :: Int -> Superlative -> [[Bool]] -> [[Bool]]
whittle bitPos which nums = filter ((== bit) . (!! bitPos)) nums
  where
    bit = findCommon which . (!! bitPos) . transpose $ nums

part2 :: [[Bool]] -> Int
part2 input = ((*) `on` go input 0) Most Least
  where
    go [num] _ _ = fromBinary num
    go nums bitPos which = go (whittle bitPos which nums) (bitPos + 1) which

day3 :: Day
day3 = Day 3 (simpleParser parser) part1 part2
