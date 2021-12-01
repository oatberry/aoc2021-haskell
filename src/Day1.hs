module Day1 (day1) where

import Common

parser :: String -> [Int]
parser = map read . lines

pairs2 :: [a] -> [(a, a)]
pairs2 [] = []
pairs2 [_] = []
pairs2 (x : y : rest) = (x, y) : pairs2 (y : rest)

pairs3 :: [a] -> [(a, a, a)]
pairs3 [] = []
pairs3 [_] = []
pairs3 [_, _] = []
pairs3 (x : y : z : rest) = (x, y, z) : pairs3 (y : z : rest)

part1 :: [Int] -> Int
part1 = length . filter (uncurry (<)) . pairs2

part2 :: [Int] -> Int
part2 = part1 . map (\(x, y, z) -> x + y + z) . pairs3

day1 :: Day
day1 = Day 1 (simpleParser parser) part1 part2
