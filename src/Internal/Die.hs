{-# LANGUAGE FlexibleContexts #-}

module Internal.Die where

import Text.Parsec
import Text.Parsec.String

import Internal.Parsing


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

instance ExpressionParser Die where
  parseExpr =
    try (dieNum "d100" D100)
    <|> try (dieNum "d20" D20)
    <|> try (dieNum "d12" D12)
    <|> try (dieNum "d10" D10)
    <|> try (dieNum "d8" D8)
    <|> try (dieNum "d6" D6)
    <|> dieNum "d4" D4
    where
      dieNum s d = string s >> return d


rangeFor :: Die -> (Int, Int)
rangeFor D4   = (1, 4)
rangeFor D6   = (1, 6)
rangeFor D8   = (1, 8)
rangeFor D10  = (1, 10)
rangeFor D12  = (1, 12)
rangeFor D20  = (1, 20)
rangeFor D100 = (1, 100)
