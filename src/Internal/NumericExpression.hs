{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Internal.NumericExpression where

import Control.Monad.Trans.Writer
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Internal.Eval
import Internal.Parsing


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


productExpression :: GenParser Char st NumericExpression
productExpression =
  try (do
    num <- number
    spaces >> char '*' >> spaces
    expr <- productExpression
    return (num `Product` expr)
  )
  <|> number

number :: GenParser Char st NumericExpression
number = Number <$> positiveDecimal
