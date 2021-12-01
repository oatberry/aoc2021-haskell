module Day1 (day1) where

import Common

parser :: String -> [Int]
parser = map read . lines

checkSpans :: Ord a => Int -> [a] -> Int
checkSpans n list =
  length
    . filter (uncurry (<))
    $ zip list (drop n list)

day1 :: Day
day1 = Day 1 (simpleParser parser) (checkSpans 1) (checkSpans 3)
