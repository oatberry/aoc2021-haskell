{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common
  ( getAOCInput,
    firstOrNothing,
    Day (..),
    runDay,
    Parser,
  )
where

import Control.Exception (IOException, try)
import qualified Data.ByteString.Char8 as C
import Data.Foldable (for_)
import Data.Maybe (listToMaybe)
import Data.Void (Void)
import Network.HTTP.Req
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (Parsec)
import Text.Printf (printf)

type Parser = Parsec Void String

data Day = Day
  { dayNum :: Int,
    dayParts :: [String -> String]
  }

runDay :: Day -> IO ()
runDay Day {dayNum, dayParts} = do
  input <- getAOCInput dayNum
  let results = zip [1 :: Int ..] $ dayParts <*> [input]
  printf "Day %d:\n" dayNum
  for_ results $ uncurry (printf "  part %d: %s\n")

getAOCInput :: Int -> IO String
getAOCInput dayNum = do
  let inputFile = "./inputs/day" ++ show dayNum
  perhapsInput <- try $ readFile inputFile
  case perhapsInput of
    Left (_ :: IOException) -> fetchInput dayNum
    Right input -> pure input

fetchInput :: Int -> IO String
fetchInput dayNum = do
  printf "Downloading input for day %d...\n" dayNum
  cookie <- getCookie
  let url = https "adventofcode.com" /: "2021" /: "day" /~ dayNum /: "input"
      headers = header "Cookie" cookie
      request = req GET url NoReqBody bsResponse headers
  response <- runReq defaultHttpConfig request
  let input = C.unpack . responseBody $ response
  writeFile ("./inputs/day" ++ show dayNum) input
  pure input

getCookie :: IO C.ByteString
getCookie = do
  perhapsCookie <- try $ C.readFile "./inputs/cookie"
  case perhapsCookie of
    Left (_ :: IOException) -> failCookie
    Right "" -> failCookie
    Right cookie -> pure cookie
  where
    failCookie = do
      hPutStrLn stderr "Error: Please put your AoC session cookie into ./inputs/cookie"
      exitFailure

firstOrNothing :: Show a => [a] -> String
firstOrNothing = maybe "Nothing" show . listToMaybe
