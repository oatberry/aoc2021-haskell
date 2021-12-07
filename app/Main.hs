module Main (main) where

import Common
import Day1
import Day2
import Day3
import Day4
import Day5

days :: [Day]
days = [day1, day2, day3, day4, day5]

main :: IO ()
main = mapM_ runDay days
