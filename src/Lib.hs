{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib where

import Prelude hiding (product)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.List
import System.Random
import Text.Parsec
import Text.Parsec.Char


-- parsing

parseLine :: String -> String -> Either ParseError CompoundExpression
parseLine = parse parser

parser = do
  spaces
  expr <- compoundExpression
  eof
  return expr

positiveDecimal :: Stream s m Char => ParsecT s u m Int
positiveDecimal = do
  init <- oneOf ['1'..'9']
  num <- many digit
  return (read (init:num) :: Int)


-- eval

type Recorder m = WriterT String m

class (Monad m) => MakesGen m where
  makeGen :: m StdGen

class (Monad m) => Eval t m where
  eval :: t -> m Int


evalLine :: (MakesGen m) => CompoundExpression -> m (Int, String)
evalLine expr = runWriterT (eval expr)


--------------------------------
-- Die
--------------------------------

-- type

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


-- parsing

die :: Stream s m Char => ParsecT s u m Die
die = 
  try (dieNum "d100" D100)
  <|> try (dieNum "d20" D20)
  <|> try (dieNum "d12" D12)
  <|> try (dieNum "d10" D10)
  <|> try (dieNum "d8" D8)
  <|> try (dieNum "d6" D6)
  <|> dieNum "d4" D4

dieNum :: Stream s m Char => String -> Die -> ParsecT s u m Die
dieNum s d = do
  string s
  return d


-- eval

rangeFor D4   = (1, 4)
rangeFor D6   = (1, 6)
rangeFor D8   = (1, 8)
rangeFor D10  = (1, 10)
rangeFor D12  = (1, 12)
rangeFor D20  = (1, 20)
rangeFor D100 = (1, 100)


--------------------------------
-- Numeric Expression
--------------------------------

-- type

data NumericExpression
  = Product NumericExpression NumericExpression
  | Number Int

instance (Monad m) => Eval NumericExpression (Recorder m) where
  eval (Product l r) = do
    nl <- eval l
    tell " * "
    nr <- eval r
    return (nl * nr)
  eval (Number i) = do
    tell (show i)
    return i


-- parsing

productExpression :: Stream s m Char => ParsecT s u m NumericExpression
productExpression =
  try (do
    num <- number
    multiply
    expr <- productExpression
    return (num `Product` expr))
  <|> number

multiply :: Stream s m Char => ParsecT s u m ()
multiply = char '*' >> spaces

number :: Stream s m Char => ParsecT s u m NumericExpression
number = do
  n <- positiveDecimal
  spaces
  return (Number n)


--------------------------------
-- Compound Expression
--------------------------------

-- type

data CompoundExpression
  = Sum CompoundExpression CompoundExpression
  | Die Int Die
  | NumericExpression NumericExpression

instance (MakesGen m, Monad m) => Eval CompoundExpression (Recorder m) where
  eval (Sum l r) = do
    nl <- eval l
    tell "  +  "
    nr <- eval r
    return (nl + nr)
  eval (Die i d) = do
    g <- lift makeGen
    let rs = take i $ randomRs (rangeFor d) g
    tell (concat [show i, show d, " (", intercalate ", " (map show rs), ")"])
    return (sum rs)
  eval (NumericExpression e) = eval e


-- parsing

compoundExpression :: Stream s m Char => ParsecT s u m CompoundExpression
compoundExpression =
  try (do
    num <- numbersOrDice
    plus
    expr <- compoundExpression
    return (num `Sum` expr))
  <|> numbersOrDice

plus :: Stream s m Char => ParsecT s u m ()
plus = char '+' >> spaces

numbersOrDice :: Stream s m Char => ParsecT s u m CompoundExpression
numbersOrDice = try dice <|> (NumericExpression <$> productExpression)

dice :: Stream s m Char => ParsecT s u m CompoundExpression
dice = do
  n <- option 1 positiveDecimal
  d <- die
  spaces
  return (Die n d)
