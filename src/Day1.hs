module Day1 (day1) where

import Common
import Data.List (tails)

parser :: String -> [Int]
parser = map read . lines

windows :: Int -> [a] -> [[a]]
windows n = filter ((== n) . length) . map (take n) . tails

part1 :: [Int] -> Int
part1 = length . filter (\[x, y] -> x < y) . windows 2

part2 :: [Int] -> Int
part2 = part1 . map (\[x, y, z] -> x + y + z) . windows 3

day1 :: Day
day1 = Day 1 (simpleParser parser) part1 part2
