{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib
 ( parseLine
 , evalLine
 , MakesGen(..)
 ) where

import Prelude hiding (product)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.List
import System.Random
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String


-- parsing

parseLine :: String -> String -> Either ParseError CompoundExpression
parseLine = parse parser

parser = do
  spaces
  expr <- compoundExpression
  spaces
  eof
  return expr

positiveDecimal :: GenParser Char st Int
positiveDecimal = do
  init <- oneOf ['1'..'9']
  num <- many digit
  return (read (init:num) :: Int)


-- eval

type Recorder m = WriterT String m

class (Monad m) => Eval t m where
  eval :: t -> m Int

class (Monad m) => MakesGen m where
  makeGen :: m StdGen


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


rangeFor D4   = (1, 4)
rangeFor D6   = (1, 6)
rangeFor D8   = (1, 8)
rangeFor D10  = (1, 10)
rangeFor D12  = (1, 12)
rangeFor D20  = (1, 20)
rangeFor D100 = (1, 100)

rolls :: (MakesGen m) => Die -> m [Int]
rolls d = randomRs (rangeFor d) <$> makeGen


-- parsing

die :: GenParser Char st Die
die = 
  try (dieNum "d100" D100)
  <|> try (dieNum "d20" D20)
  <|> try (dieNum "d12" D12)
  <|> try (dieNum "d10" D10)
  <|> try (dieNum "d8" D8)
  <|> try (dieNum "d6" D6)
  <|> dieNum "d4" D4

dieNum :: String -> Die -> GenParser Char st Die
dieNum s d = string s >> return d


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

productExpression :: GenParser Char st NumericExpression
productExpression =
  try (do
    num <- number
    spaces >> char '*' >> spaces
    expr <- productExpression
    return (num `Product` expr))
  <|> number

number :: GenParser Char st NumericExpression
number = Number <$> positiveDecimal


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
  eval (Die n d) = do
    rs <- take n <$> lift (rolls d)
    tell (concat [show n, show d, " (", intercalate ", " (map show rs), ")"])
    return (sum rs)
  eval (NumericExpression e) = eval e


-- parsing

compoundExpression :: GenParser Char st CompoundExpression
compoundExpression =
  try (do
    num <- numbersOrDice
    spaces >> char '+' >> spaces
    expr <- compoundExpression
    return (num `Sum` expr))
  <|> numbersOrDice

numbersOrDice :: GenParser Char st CompoundExpression
numbersOrDice = try dice <|> (NumericExpression <$> productExpression)

dice :: GenParser Char st CompoundExpression
dice = Die <$> option 1 positiveDecimal <*> die
