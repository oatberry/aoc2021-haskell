{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Day8 (day8) where

import Common
import Control.Lens
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char

type Entry = ([String], [String])

data Segment = T | TL | TR | M | BL | BR | B
  deriving (Show, Eq, Enum, Ord)

parser :: Parser [Entry]
parser = many (entry <* newline)
  where
    entry = (,) <$> (segments <* string "| ") <*> segments
    segments = many $ some (oneOf "abcdefg") <* hspace

part1 :: [Entry] -> Int
part1 =
  lengthOf $
    folded . _2 . folded . filtered ((`elem` [2, 3, 4, 7]) . length)

invertAssignments :: Map Segment (Set Char) -> Map Char Segment
invertAssignments m =
  Map.fromList
    [ (head (Set.toList chars), seg)
      | (seg, chars) <- Map.toList m,
        length chars == 1
    ]

deduceAssignments :: [String] -> Map Char Segment
deduceAssignments jumbled = invertAssignments $ Map.foldlWithKey' adjust start grouped
  where
    adjust candidates len chars =
      Map.mapWithKey
        (whittleCandidates len chars)
        candidates
    whittleCandidates len chars seg maybes =
      if seg `Set.member` lengthsToSegments len
        then maybes `Set.intersection` chars
        else maybes `Set.difference` chars
    start = Map.fromList [(seg, Set.fromList "abcdefg") | seg <- [T .. B]]
    grouped =
      Map.fromListWith Set.intersection [(length a, Set.fromList a) | a <- jumbled]
    lengthsToSegments =
      Set.fromList . \case
        2 -> [TR, BR]
        3 -> [T, TR, BR]
        4 -> [TL, M, TR, BR]
        5 -> [T, M, B]
        6 -> [TL, T, BR, B]
        _ -> [T .. B]

decode7Seg :: Map Char Segment -> String -> Char
decode7Seg assignments = decode . map (assignments !)
  where
    decode segs = case length segs of
      2 -> '1'
      3 -> '7'
      4 -> '4'
      5 ->
        if
            | TL `elem` segs -> '5'
            | BL `elem` segs -> '2'
            | otherwise -> '3'
      6 ->
        if
            | TR `notElem` segs -> '6'
            | BL `notElem` segs -> '9'
            | otherwise -> '0'
      _ -> '8'

decodeEntry :: Entry -> Int
decodeEntry (lhs, rhs) = read . map (decode7Seg assignments) $ rhs
  where
    assignments = deduceAssignments lhs

part2 :: [Entry] -> Int
part2 = sum . map decodeEntry

day8 :: Day
day8 = Day 8 parser part1 part2
