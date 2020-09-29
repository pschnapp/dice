module Lib
 ( parseLine
 , evalLine
 , MakesGen(..)
 ) where

import Control.Monad.Trans.Writer
import Text.Parsec

import Internal.CompoundExpression
import Internal.Dice
import Internal.Eval


parseLine :: String -> String -> Either ParseError CompoundExpression
parseLine = parse $ do
  expr <- compoundExpression
  eof
  return expr

evalLine :: (MakesGen m) => CompoundExpression -> m (Int, String)
evalLine expr = runWriterT (eval expr)
