module Main (main) where

import Common
import Day1
import Day10
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9

days :: [Day]
days = [day1, day2, day3, day4, day5, day6, day7, day8, day9, day10]

main :: IO ()
main = mapM_ runDay days
