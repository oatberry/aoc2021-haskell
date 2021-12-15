{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Day10 (day10) where

import Common
import Control.Lens
import Data.List (foldl', sort)
import Text.Megaparsec
import Text.Megaparsec.Char

parser :: Parser [String]
parser = many (many (oneOf "()[]{}<>") <* newline)

isOpening :: Char -> Bool
isOpening c = c `elem` "([{<"

opener :: Char -> Char
opener = \case ')' -> '('; ']' -> '['; '}' -> '{'; _ -> '<'

closer :: Char -> Char
closer = \case '(' -> ')'; '[' -> ']'; '{' -> '}'; _ -> '>'

data Result = Illegal Char | Incomplete [Char]
  deriving (Show)

makePrisms ''Result

analyzeLine :: String -> Result
analyzeLine = go []
  where
    go stack [] = Incomplete (map closer stack)
    go stack (next : rest) =
      if isOpening next
        then go (next : stack) rest
        else case stack of
          (top : stackRest) | top == opener next -> go stackRest rest
          _ -> Illegal next

part1 :: [String] -> Int
part1 = sumOf (folded . _Illegal . to numberize) . map analyzeLine
  where
    numberize = \case ')' -> 3; ']' -> 57; '}' -> 1197; _ -> 25137

part2 :: [String] -> Int
part2 = median . map numberize . toListOf (folded . _Incomplete) . map analyzeLine
  where
    numberize = foldl' (\a c -> 5 * a + (case c of ')' -> 1; ']' -> 2; '}' -> 3; _ -> 4)) 0
    median nums = sort nums !! (length nums `div` 2)

day10 :: Day
day10 = Day 10 parser part1 part2
