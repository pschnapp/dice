{-# LANGUAGE FlexibleContexts #-}

module Lib where

import System.Random
import Text.Parsec
import Text.Parsec.Char


parseLine source text = parse parser source text


data Die
  = D4
  | D6
  | D8
  | D10
  | D12
  | D20
  | D100

instance Show Die where
  show D4   = "d4"
  show D6   = "d6"
  show D8   = "d8"
  show D10  = "d10"
  show D12  = "d12"
  show D20  = "d20"
  show D100 = "d100"

class Range a where
  rangeFor :: a -> (Int, Int)

instance Range Die where
  rangeFor D4   = (1, 4)
  rangeFor D6   = (1, 6)
  rangeFor D8   = (1, 8)
  rangeFor D10  = (1, 10)
  rangeFor D12  = (1, 12)
  rangeFor D20  = (1, 20)
  rangeFor D100 = (1, 100)

rolls :: (RandomGen g) => Die -> g -> [Int]
rolls = randomRs . rangeFor


data Item
  = Number Int
  | Die Int Die
  deriving (Show)


positiveDecimal :: Stream s m Char => ParsecT s u m Int
positiveDecimal = do
  init <- oneOf ['1'..'9']
  num <- many digit
  return (read (init:num) :: Int)

dieCount :: Stream s m Char => ParsecT s u m Int
dieCount = option 1 positiveDecimal

dieNum :: Stream s m Char => String -> Die -> ParsecT s u m Die
dieNum s d = do
  string s
  return d

die :: Stream s m Char => ParsecT s u m Die
die = 
  try (dieNum "d100" D100)
  <|> try (dieNum "d20" D20)
  <|> try (dieNum "d12" D12)
  <|> try (dieNum "d10" D10)
  <|> try (dieNum "d8" D8)
  <|> try (dieNum "d6" D6)
  <|> dieNum "d4" D4

dice :: Stream s m Char => ParsecT s u m Item
dice = do
  n <- dieCount
  d <- die
  return (Die n d)

number :: Stream s m Char => ParsecT s u m Item
number = Number <$> positiveDecimal

numOrDice :: Stream s m Char => ParsecT s u m Item
numOrDice = try dice <|> number

plus :: Stream s m Char => ParsecT s u m ()
plus = spaces >> char '+' >> spaces

parser :: Stream s m Char => ParsecT s u m [Item]
parser = do
  spaces
  items <- numOrDice `sepBy1` plus
  spaces
  return items
