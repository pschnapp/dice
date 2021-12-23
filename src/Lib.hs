module Lib
 ( parseLine
 , evalLine
 , MakesGen(..)
 ) where

import Control.Monad.Trans.Writer
import Data.Either.Combinators
import Text.Parsec

import Internal.Eval
import Internal.Expression
import Internal.Parsing


parseLine :: String -> String -> Either String Expression
parseLine source = mapLeft show . parse lineParser source

evalLine :: (MakesGen m) => Expression -> m (Int, String)
evalLine = runWriterT . eval
