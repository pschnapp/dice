{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Internal.CompoundExpression where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.List
import Text.Parsec
import Text.Parsec.String

import Internal.Die
import Internal.Eval
import Internal.NumericExpression
import Internal.Parsing


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


compoundExpression :: GenParser Char st CompoundExpression
compoundExpression =
  try (do
    nod <- numbersOrDice
    spaces >> char '+' >> spaces
    expr <- compoundExpression
    return (nod `Sum` expr))
  <|> numbersOrDice

numbersOrDice :: GenParser Char st CompoundExpression
numbersOrDice = try dice <|> (NumericExpression <$> productExpression)

dice :: GenParser Char st CompoundExpression
dice = Die <$> option 1 positiveDecimal <*> die
