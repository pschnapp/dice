{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Internal.Eval
 ( MakesGen(..)
 , eval
 ) where

import Control.Monad.Writer
import Data.Functor.Identity
import Data.List
import System.Random

import Internal.Expression


--------------------------------
-- Expression Tagging
--------------------------------

-- Die ops are tagged as Dynamic since they need to be re-run when part of
-- a `Times` operator expression; this tagging propagates up the AST

data DynamicsTag a
  = Dynamic { unwrap :: a }
  | Static { unwrap :: a }

type TaggedExpression = DynamicsTag (Expression DynamicsTag)


tag :: IdentityExpression -> TaggedExpression
tag (Identity (Binary op l r))
 | isDieOp op = Dynamic (Binary op (tag l) (tag r))
 | otherwise =
  let tl = tag l; tr = tag r in case (tl, tr) of
    (Dynamic _, Dynamic _) -> Dynamic (Binary op tl tr)
    (Dynamic _, Static  _) -> Dynamic (Binary op tl tr)
    (Static  _, Dynamic _) -> Dynamic (Binary op tl tr)
    (Static  _, Static  _) -> Static  (Binary op tl tr)
tag (Identity (Unary op e))
 | isDieOp op = Dynamic (Unary op (tag e))
 | otherwise =
  let tagged = tag e in case tagged of
    Dynamic _ -> Dynamic (Unary op tagged)
    Static  _ -> Static (Unary op tagged)
tag (Identity (Expression e)) =
  let tagged = tag e in case tagged of
    Dynamic _ -> Dynamic (Expression tagged)
    Static  _ -> Static (Expression tagged)
tag (Identity (Term c)) = Static (Term c)


--------------------------------
-- Evaluation
--------------------------------

class (Monad m) => MakesGen m where
  makeGen :: m StdGen

instance (MakesGen m) => MakesGen (WriterT String m) where
  makeGen = lift makeGen

-- type-class synonym
class (MakesGen m, MonadWriter String m) => EvalM m where {}
instance (MakesGen m, MonadWriter String m) => EvalM m where {}


data RollType
  = Normal
  | WithAdvantage
  | WithDisadvantage


eval :: EvalM m => IdentityExpression -> m Int
eval = doEval . tag

doEval :: EvalM m => TaggedExpression -> m Int
doEval te =
  case unwrap te of
    Binary Times d@Dynamic{} s@Static{} -> evalTimesOperation s d
    Binary Times s@Static{} d@Dynamic{} -> evalTimesOperation s d
    Binary op l r -> evalBinaryOperation op l r
    Unary Negation e -> do
      tell "-"
      ((-1)*) <$> doEval e
    Unary UnaryDie e ->
      evalBinaryOperation BinaryDie (Static $ Term 1) e
    Unary UnaryDieWithAdvantage e ->
      evalBinaryOperation BinaryDieWithAdvantage (Static $ Term 1) e
    Unary UnaryDieWithDisadvantage e ->
      evalBinaryOperation BinaryDieWithDisadvantage (Static $ Term 1) e
    Expression e -> do
      tell "("
      v <- doEval e
      tell ")"
      return v
    Term c -> do
      tell (show c)
      return (fromIntegral c)

evalBinaryOperation :: EvalM m => BinaryOperator -> TaggedExpression -> TaggedExpression -> m Int
evalBinaryOperation op l r = do
  vl <- doEval l
  tell $ concat [" ", show op, " "]
  vr <- doEval r
  runBinaryOp op vl vr

evalTimesOperation :: EvalM m => TaggedExpression -> TaggedExpression -> m Int
evalTimesOperation s d = do
  tell "{("
  n <- doEval s
  if isTerm (unwrap s)
  then tell $ concat [" ", show Times, "): "]
  else tell $ concat [" = ", show n, " ", show Times, "): "]
  v <- (signum n *) <$> runTimes (abs n) d -- hide sign of num then reapply
  tell ("}=" ++ show v)
  return v

runTimes :: EvalM m => Int -> TaggedExpression -> m Int
runTimes n e =
  sum <$> sequence (intersperse showPlus . replicate n $ evalShowingResult e)
  where showPlus = tell " + " >> return 0

evalShowingResult :: EvalM m => TaggedExpression -> m Int
evalShowingResult e = do
  tell "["
  v <- doEval e
  tell ("]=" ++ show v)
  return v

runBinaryOp :: EvalM m => BinaryOperator -> Int -> Int -> m Int
runBinaryOp = \case
  Addition -> pureBinary (+)
  BinaryDie -> dieOperation Normal
  BinaryDieWithAdvantage -> dieOperation WithAdvantage
  BinaryDieWithDisadvantage -> dieOperation WithDisadvantage
  Multiplication -> pureBinary (*)
  Subtraction -> pureBinary (-)
  Times -> pureBinary (*)

pureBinary :: Monad m => (a -> a -> a) -> a -> a -> m a
pureBinary op l r = return (l `op` r)

dieOperation :: EvalM m => RollType -> Int -> Int -> m Int
dieOperation t n d = do
  (rs, shown) <- rolls (abs n) (abs d) t -- only use positive `n` and `d` here
  let total = signum n * signum d * sum rs -- reapply sign of `n` and `d` to get result
  let negSum = if total < 0 then "-" else ""
  tell $ concat [" (", negSum,  "Σ[", intercalate "," shown, "]=", show total, ")"] -- U+03A3 Σ
  return total

rolls :: (MakesGen m) => Int -> Int -> RollType -> m ([Int], [String])
rolls 0 _ = \_ -> return ([], [])
rolls n 0 = \_ -> return (replicate n 0, replicate n "0")
rolls n d = \case
  Normal -> do
    rs <- doRolls
    return (rs, map show rs)
  WithAdvantage -> do
    pairs <- rollPairs
    return (map snd pairs, map showGT pairs)
  WithDisadvantage -> do
    pairs <- rollPairs
    return (map fst pairs, map showLT pairs)
  where
    doRolls = take n . randomRs (1, d) <$> makeGen
    orderPair a b = (min a b, max a b)
    rollPairs = zipWith orderPair <$> doRolls <*> doRolls
    showLT (a, b) = concat ["(", show a, "<", show b, ")"]
    showGT (a, b) = concat ["(", show b, ">", show a, ")"]
