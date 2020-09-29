{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Internal.CompoundExpression where

import Control.Monad.Trans.Writer
import Text.Parsec
import Text.Parsec.String

import Internal.Dice
import Internal.Eval
import Internal.NumericExpression
import Internal.Parsing


data CompoundExpression
  = Sum CompoundExpression CompoundExpression
  | DiceExpression Dice
  | NumericExpression NumericExpression

instance (MakesGen m, Monad m) => Eval CompoundExpression (Recorder m) where
  eval (Sum l r) = do
    nl <- eval l
    tell "  +  "
    nr <- eval r
    return (nl + nr)
  eval (DiceExpression e) = eval e
  eval (NumericExpression e) = eval e

instance ExpressionParser CompoundExpression where
  parseExpr =
    try (do
      nod <- numbersOrDice
      spaces >> char '+' >> spaces
      expr <- parseExpr
      return (nod `Sum` expr))
    <|> numbersOrDice
    where
      numbersOrDice =
        try (DiceExpression <$> parseExpr)
        <|> (NumericExpression <$> parseExpr)
