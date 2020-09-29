module Internal.Parsing where

import Text.Parsec
import Text.Parsec.String


positiveDecimal :: GenParser Char st Int
positiveDecimal = do
  init <- oneOf ['1'..'9']
  num <- many digit
  return (read (init:num) :: Int)
