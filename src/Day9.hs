{-# LANGUAGE TupleSections #-}

module Day9 (day9) where

import Common
import Control.Lens
import Data.Char
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Text.Megaparsec
import Text.Megaparsec.Char

type Height = Int

type Point = (Int, Int)

type HeightMap = Vector (Vector Height)

parser :: Parser HeightMap
parser = Vec.fromList <$> many (row <* newline)
  where
    row = Vec.fromList <$> many (digitToInt <$> digitChar)

neighbors :: HeightMap -> Point -> [(Point, Height)]
neighbors hm (x, y) = mapMaybe ixHM [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  where
    ixHM (a, b) = ((a, b),) <$> (hm ^? ix a . ix b)

lowPoints :: HeightMap -> [(Point, Height)]
lowPoints hm = hm ^.. (ifolded <.> ifolded) . ifiltered isLowPoint . withIndex
  where
    isLowPoint point height = all ((> height) . snd) (neighbors hm point)

part1 :: HeightMap -> Int
part1 = sum . map ((+ 1) . snd) . lowPoints

findBasin :: HeightMap -> Point -> Set Point
findBasin heightMap = go Set.empty
  where
    go seen point = foldl looky (Set.insert point seen) (neighbors heightMap point)
    looky seen (point, height) =
      if point `Set.member` seen || height == 9
        then seen
        else go seen point

part2 :: HeightMap -> Maybe Int
part2 = ans . sortBy (flip compare `on` length) . basins
  where
    basins heightMap = map (findBasin heightMap . fst) . lowPoints $ heightMap
    ans (a : b : c : _) = Just $ length a * length b * length c
    ans _ = Nothing

day9 :: Day
day9 = Day 9 parser part1 part2
