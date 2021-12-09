{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common
  ( Day (..),
    Parser,
    simpleParser,
    testParser,
    runDay,
    runDayWithInput,
    getAOCInput,
  )
where

import Control.Exception (IOException, try)
import qualified Data.ByteString.Char8 as C
import Data.Void (Void)
import Network.HTTP.Req
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (Parsec, eof, parse, parseMaybe, takeRest)
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Printf (printf)

data Day where
  Day ::
    (Show out1, Show out2) =>
    -- | day number
    Int ->
    -- | parser
    Parser input ->
    -- | part 1 solution
    (input -> out1) ->
    -- | part 2 solution
    (input -> out2) ->
    Day

type Parser = Parsec Void String

simpleParser :: (String -> a) -> Parser a
simpleParser parser = parser <$> takeRest

testParser :: Parser a -> String -> Maybe a
testParser p = parseMaybe (p <* space <* eof)

runDay :: Day -> IO ()
runDay day@(Day dayNum _ _ _) = getAOCInput dayNum >>= runDayWithInput day

runDayWithInput :: Day -> String -> IO ()
runDayWithInput (Day dayNum parser part1 part2) rawInput = do
  printf "Day %d:\n" dayNum
  case parse (parser <* space <* eof) "input" rawInput of
    Left e -> putStrLn $ errorBundlePretty e
    Right input -> do
      printf "  part 1: %s\n" . show . part1 $ input
      printf "  part 2: %s\n" . show . part2 $ input

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
