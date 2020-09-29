{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Internal.Dice where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.List
import System.Random
import Text.Parsec
import Text.Parsec.String

import Internal.Die
import Internal.Eval
import Internal.Parsing


class (Monad m) => MakesGen m where
  makeGen :: m StdGen

data Dice = Dice Int Die

instance (MakesGen m, Monad m) => Eval Dice (Recorder m) where
  eval (Dice n d) = do
    rs <- take n  . randomRs (rangeFor d) <$> lift makeGen
    tell (concat [show n, show d, " (", intercalate ", " (map show rs), ")"])
    return (sum rs)

instance ExpressionParser Dice where
  parseExpr = Dice <$> option 1 positiveDecimal <*> parseExpr
