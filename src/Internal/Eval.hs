{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Internal.Eval where

import Control.Monad.Writer
import Data.List
import System.Random

import Internal.Expression


class (Monad m) => MakesGen m where
  makeGen :: m StdGen

instance (MakesGen m) => MakesGen (WriterT String m) where
  makeGen = lift makeGen

data RollType
  = Normal
  | WithAdvantage
  | WithDisadvantage


-- want multiplcation to do rerolls of dice
-- other than that the evaluation is pretty straight-forward

eval :: (MakesGen m, MonadWriter String m) => Expression -> m Int
eval = \case
  Binary op@Addition l r ->
    evalBinaryOperation op l r (pureBinary (+))
  Binary op@Subtraction l r ->
    evalBinaryOperation op l r (pureBinary (-))
  Binary op@Multiplication l r ->
    evalBinaryOperation op l r (pureBinary (*))
  Binary op@BinaryDie l r ->
    evalBinaryOperation op l r (dieOperation Normal)
  Binary op@BinaryDieWithAdvantage l r ->
    evalBinaryOperation op l r (dieOperation WithAdvantage)
  Binary op@BinaryDieWithDisadvantage l r ->
    evalBinaryOperation op l r (dieOperation WithDisadvantage)
  Unary Negation e -> do
    tell "-"
    ((-1)*) <$> eval e
  Unary UnaryDie e ->
    evalBinaryOperation BinaryDie (Term 1) e (dieOperation Normal)
  Unary UnaryDieWithAdvantage e ->
    evalBinaryOperation BinaryDieWithAdvantage (Term 1) e (dieOperation WithAdvantage)
  Unary UnaryDieWithDisadvantage e ->
    evalBinaryOperation BinaryDieWithDisadvantage (Term 1) e (dieOperation WithDisadvantage)
  Expression e -> do
    tell "("
    v <- eval e
    tell ")"
    return v
  Term c -> do
    tell (show c)
    return (fromIntegral c)

evalBinaryOperation
  :: (MakesGen m, MonadWriter String m)
  => BinaryOperator -> Expression -> Expression -> (Int -> Int -> m Int) -> m Int
evalBinaryOperation op l r doOp = do
  vl <- eval l
  tell $ concat [" ", show op, " "]
  vr <- eval r
  doOp vl vr

pureBinary :: Monad m => (a -> a -> a) -> a -> a -> m a
pureBinary op l r = return (l `op` r)

dieOperation
  :: (MakesGen m, MonadWriter String m)
  => RollType -> Int -> Int -> m Int
dieOperation t n d = do
  (rs, shown) <- rolls (abs n) (abs d) t -- only use positive `n` and `d` here
  let total = signum n * signum d * sum rs -- reapply sign of `n` and `d` to result
  let negSum = if total < 0 then "-" else ""
  tell $ concat [" (", negSum,  "Σ[", intercalate "," shown, "]=", show total, ")"] -- U+03A3 Σ
  return total

rolls :: (MakesGen m) => Int -> Int -> RollType -> m ([Int], [String])
rolls 0 _ = \_ -> return ([], [])
rolls n 0 = \_ -> return (replicate n 0, replicate n "0")
rolls n d = \case
  Normal -> do
    rs <- doRolls
    let shown = map show rs
    return (rs, shown)
  WithAdvantage -> do
    pairs <- rollPairs
    let rs = snd $ unzip pairs
    let shown = map showGT pairs
    return (rs, shown)
  WithDisadvantage -> do
    pairs <- rollPairs
    let rs = fst $ unzip pairs
    let shown = map showLT pairs
    return (rs, shown)
  where
    doRolls = take n . randomRs (1, d) <$> makeGen
    rollPairs = do
      rs1 <- doRolls
      rs2 <- doRolls
      return $ zipWith minMaxPair rs1 rs2
    minMaxPair a b = (min a b, max a b)
    showLT (a, b) = concat ["(", show a, "<", show b, ")"]
    showGT (a, b) = concat ["(", show b, ">", show a, ")"]
