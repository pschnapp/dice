{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module Internal.Parsing where

import Text.Parsec
import Text.Parsec.String

import Internal.Expression


data Associativity
  = LeftAssociative
  | RightAssociative

class HasAssociativity a where
  associativityOf :: a -> Associativity

instance HasAssociativity BinaryOperator where
  associativityOf Addition = LeftAssociative
  associativityOf BinaryDie = LeftAssociative
  associativityOf BinaryDieWithAdvantage = LeftAssociative
  associativityOf BinaryDieWithDisadvantage = LeftAssociative
  associativityOf Multiplication = LeftAssociative
  associativityOf Subtraction = LeftAssociative



class HasPrecedence a where
  precedenceOf :: a -> Word

instance HasPrecedence BinaryOperator where
  precedenceOf Addition = 0
  precedenceOf BinaryDie = 3
  precedenceOf BinaryDieWithAdvantage = 3
  precedenceOf BinaryDieWithDisadvantage = 3
  precedenceOf Multiplication = 1
  precedenceOf Subtraction = 0

instance HasPrecedence UnaryOperator where
  precedenceOf Negation = 5
  precedenceOf UnaryDie = 4
  precedenceOf UnaryDieWithAdvantage = 4
  precedenceOf UnaryDieWithDisadvantage = 4


type ExpressionParser st = GenParser Char st Expression


lineParser :: ExpressionParser st
lineParser = do
  e <- expressionParser
  eof
  return e

-- using the Precedence Climbing algorithm for parsing, as found here:
-- https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#climbing

-- corresponds to `E` in the paper
expressionParser :: ExpressionParser st
expressionParser = expr 0

-- corresponds to `Exp` in the paper
expr :: Word -> ExpressionParser st
expr prec = padded term >>= expr' prec

expr' :: Word -> Expression -> ExpressionParser st
expr' prec p = do
  b <- peek binaryOp -- don't consume it yet, since we might not use it
  case b of
    Nothing -> return p
    Just op | precedenceOf op < prec -> return p
    Just op -> do
      _ <- binaryOp -- now consume it, since we're using it
      pr <- expr case associativityOf op of
              LeftAssociative -> precedenceOf op + 1
              RightAssociative -> precedenceOf op
      expr' prec (Binary op p pr)

-- corresponds to `P` in the paper
term :: ExpressionParser st
term =
  do
    op <- unaryOp
    e <- expr (precedenceOf op)
    return (Unary op e)
  <|> Expression <$> parens expressionParser
  <|> Term <$> constant

-- corresponds to `B` in the paper
binaryOp = operatorFrom Addition
-- corresponds to `U` in the paper
unaryOp = operatorFrom Negation

operatorFrom :: (Enum a, Show a) => a -> GenParser Char st a
operatorFrom init =
  let opParser op = string (show op) >> return op
  in foldl1 (<|>) $ map opParser (enumFrom init)

constant :: GenParser Char st Constant
constant = do
  num <- many1 digit
  return (read num :: Constant)

padded = between spaces spaces
parens = between (char '(') (char ')')
peek = optionMaybe . lookAhead . try
