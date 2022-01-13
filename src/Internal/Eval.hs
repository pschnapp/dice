{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

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

data DynamicsTag a
  = Dynamic a
  | Static a

type UntaggedInner = Expression Identity
type TaggedInner = Expression DynamicsTag
type TaggedExpression = DynamicsTag (Expression DynamicsTag)


unwrap :: TaggedExpression -> TaggedInner
unwrap (Dynamic a) = a
unwrap (Static  a) = a

tagSubExprs :: UntaggedInner -> TaggedInner
tagSubExprs e = unwrap $ tagSubExprs' e

tagSubExprs' :: Expression Identity -> TaggedExpression
tagSubExprs' (Binary op (Identity l) (Identity r))
 | isDieOp op = Dynamic (Binary op (tagSubExprs' l) (tagSubExprs' r))
 | otherwise =
  let tl = tagSubExprs' l; tr = tagSubExprs' r in case (tl, tr) of
    (Dynamic _, Dynamic _) -> Dynamic (Binary op tl tr)
    (Dynamic _, Static  _) -> Dynamic (Binary op tl tr)
    (Static  _, Dynamic _) -> Dynamic (Binary op tl tr)
    (Static  _, Static  _) -> Static  (Binary op tl tr)
tagSubExprs' (Unary op (Identity e))
 | isDieOp op = Dynamic (Unary op (tagSubExprs' e))
 | otherwise =
  let tagged = tagSubExprs' e in case tagged of
    Dynamic _ -> Dynamic (Unary op tagged)
    Static  _ -> Static (Unary op tagged)
tagSubExprs' (Expression (Identity e)) =
  let tagged = tagSubExprs' e in case tagged of
    Dynamic _ -> Dynamic (Expression tagged)
    Static  _ -> Static (Expression tagged)
tagSubExprs' (Term c) = Static (Term c)


--------------------------------
-- Evaluation
--------------------------------

class (Monad m) => MakesGen m where
  makeGen :: m StdGen

instance (MakesGen m) => MakesGen (WriterT String m) where
  makeGen = lift makeGen


data RollType
  = Normal
  | WithAdvantage
  | WithDisadvantage


eval :: (MakesGen m, MonadWriter String m) => IdentityExpression -> m Int
eval = eval' . tagSubExprs . runIdentity

eval' :: (MakesGen m, MonadWriter String m) => TaggedInner -> m Int
eval' = \case
  Binary op@Addition l r ->
    evalBinaryOperation op (unwrap l) (unwrap r) (pureBinary (+))
  Binary op@Subtraction l r ->
    evalBinaryOperation op (unwrap l) (unwrap r) (pureBinary (-))
  Binary op@Multiplication l r ->
    evalBinaryOperation op (unwrap l) (unwrap r) (pureBinary (*))
  Binary op@BinaryDie l r ->
    evalBinaryOperation op (unwrap l) (unwrap r) (dieOperation Normal)
  Binary op@BinaryDieWithAdvantage l r ->
    evalBinaryOperation op (unwrap l) (unwrap r) (dieOperation WithAdvantage)
  Binary op@BinaryDieWithDisadvantage l r ->
    evalBinaryOperation op (unwrap l) (unwrap r) (dieOperation WithDisadvantage)
  Binary op@Times l r ->
    evalTimesOperation l r
  Unary Negation e -> do
    tell "-"
    ((-1)*) <$> eval' (unwrap e)
  Unary UnaryDie e ->
    evalBinaryOperation BinaryDie (Term 1) (unwrap e) (dieOperation Normal)
  Unary UnaryDieWithAdvantage e ->
    evalBinaryOperation BinaryDieWithAdvantage (Term 1) (unwrap e) (dieOperation WithAdvantage)
  Unary UnaryDieWithDisadvantage e ->
    evalBinaryOperation BinaryDieWithDisadvantage (Term 1) (unwrap e) (dieOperation WithDisadvantage)
  Expression e -> do
    tell "("
    v <- eval' (unwrap e)
    tell ")"
    return v
  Term c -> do
    tell (show c)
    return (fromIntegral c)

evalBinaryOperation
  :: (MakesGen m, MonadWriter String m)
  => BinaryOperator -> TaggedInner -> TaggedInner -> (Int -> Int -> m Int) -> m Int
evalBinaryOperation op l r doOp = do
  vl <- eval' l
  tell $ concat [" ", show op, " "]
  vr <- eval' r
  doOp vl vr

evalTimesOperation
  :: (MakesGen m, MonadWriter String m)
  => TaggedExpression -> TaggedExpression -> m Int
evalTimesOperation (Dynamic l) (Dynamic r) =
  evalBinaryOperation Times l r (pureBinary (*))
evalTimesOperation (Dynamic d) (Static  s) =
  evalTimesOperation' s d
evalTimesOperation (Static  s) (Dynamic d) =
  evalTimesOperation' s d
evalTimesOperation (Static  l) (Static  r) =
  evalBinaryOperation Times l r (pureBinary (*))

evalTimesOperation'
  :: (MakesGen m, MonadWriter String m)
  => TaggedInner -> TaggedInner -> m Int
evalTimesOperation' s d = do
  tell "{("
  n <- eval' s
  if isTerm s
  then tell $ concat [" ", show Times, "): "]
  else tell $ concat [" = ", show n, " ", show Times, "): "]
  v <- (signum n *) <$> runTimes (abs n) d -- hide sign of num then reapply
  tell $ concat ["}=", show v]
  return v

runTimes
  :: (MakesGen m, MonadWriter String m)
  => Int -> TaggedInner -> m Int
runTimes n e = do
  let es = replicate n (evalShowingResult e)
  let es' = intersperse showPlus es
  sum <$> sequence es'

evalShowingResult :: (MakesGen m, MonadWriter String m) => TaggedInner -> m Int
evalShowingResult e = do
  tell "["
  v <- eval' e
  tell $ concat ["]=", show v]
  return v

showPlus :: (MakesGen m, MonadWriter String m) => m Int
showPlus = tell " + " >> return 0

pureBinary :: Monad m => (a -> a -> a) -> a -> a -> m a
pureBinary op l r = return (l `op` r)

dieOperation
  :: (MakesGen m, MonadWriter String m)
  => RollType -> Int -> Int -> m Int
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
