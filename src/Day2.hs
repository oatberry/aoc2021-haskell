{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day2 (day2) where

import Common
import Control.Lens
import Control.Monad.Trans.State
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

data Submarine = Submarine {_hPos :: Int, _depth :: Int, _aim :: Int}
  deriving (Show, Eq)

makeLenses ''Submarine

type SubCommand = State Submarine ()

parser :: Parser SubCommand
parser = sequence_ <$> many (subCommand <* newline)
  where
    subCommand =
      ("forward " *> fmap forward decimal)
        <|> ("down " *> fmap down decimal)
        <|> ("up " *> fmap up decimal)

    forward x = do
      aim' <- use aim
      hPos += x
      depth += (aim' * x)
    down x = aim += x
    up x = aim -= x

driveSub :: Getting Int Submarine Int -> SubCommand -> Int
driveSub final commands = evalState (commands >> getValue) $ Submarine 0 0 0
  where
    getValue = (*) <$> use hPos <*> use final

day2 :: Day
day2 = Day 2 parser (driveSub aim) (driveSub depth)
