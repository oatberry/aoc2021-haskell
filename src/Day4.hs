{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day4 (day4) where

import Common
import Control.Lens
import Data.Distributive
import Data.Function
import Data.List (find)
import Data.Monoid
import qualified Data.Vector as Vec
import Linear.V
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Row = (V 5 (Int, Bool))

type Board = V 5 Row

data Bingo = Bingo {_nums :: [Int], _boards :: [Board]}

parser :: Parser Bingo
parser = Bingo <$> (numsP <* "\n\n") <*> (boardP `sepBy` "\n\n")
  where
    numsP = decimal `sepBy` ","
    boardP = V <$> Vec.replicateM 5 (skipMany "\n" *> rowP) :: Parser Board
    rowP = V <$> Vec.replicateM 5 (space *> fmap (,False) decimal) :: Parser Row

bingoRounds :: Bingo -> [(Int, [Board])]
bingoRounds Bingo {_nums, _boards} = zip _nums . drop 1 $ scanl playRound _boards _nums
  where
    playRound boards number =
      boards
        & filter (not . isSolved)
        & set (cells . filtered ((== number) . fst) . _2) True
    cells = mapped . mapped . mapped

boardScore :: Int -> Board -> Int
boardScore num board = num * sumOf unmarkedCells board
  where
    unmarkedCells = folded . folded . filtered (not . snd) . _1

isSolved :: Board -> Bool
isSolved board = ((||) `on` any (all snd)) board (distribute board)

playBingo :: Monoid c => (Maybe Int -> c) -> Bingo -> c
playBingo monoid = foldMap findBoard . bingoRounds
  where
    findBoard (num, boards) = monoid . fmap (boardScore num) . find isSolved $ boards

day4 :: Day
day4 = Day 4 parser (getFirst . playBingo First) (getLast . playBingo Last)
