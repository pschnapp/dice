{-# LANGUAGE MultiParamTypeClasses #-}

module Internal.Eval where

import Control.Monad.Trans.Writer
import System.Random


type Recorder m = WriterT String m

class (Monad m) => Eval t m where
  eval :: t -> m Int
