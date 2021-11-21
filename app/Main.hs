module Main (main) where

import Common
import Day1

days :: [Day]
days = [day1]

main :: IO ()
main = mapM_ runDay days
