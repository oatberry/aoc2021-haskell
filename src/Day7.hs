module Day7 (day7) where

import Common
import Data.List (sort)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

parser :: Parser [Int]
parser = decimal `sepBy` string ","

part1 :: [Int] -> Int
part1 nums = sum $ map (abs . subtract median) nums
  where
    median = sort nums !! (length nums `div` 2)

part2 :: [Int] -> Int
part2 nums = sum $ map (triangular . abs . subtract mean) nums
  where
    mean = sum nums `div` length nums
    triangular n = (n ^ (2 :: Int) + n) `div` 2

day7 :: Day
day7 = Day 7 parser part1 part2
