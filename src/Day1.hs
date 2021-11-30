module Day1 (day1) where

import Common

parser :: String -> String
parser = id

part1 :: String -> ()
part1 = const ()

part2 :: String -> ()
part2 = const ()

day1 :: Day
day1 = Day 1 (simpleParser parser) part1 part2
