module Internal.Expression where

import Data.Functor.Identity


type Constant = Word


class DieOp a where
  isDieOp :: a -> Bool


data BinaryOperator
  = Addition
  | BinaryDie
  | BinaryDieWithAdvantage
  | BinaryDieWithDisadvantage
  | Multiplication
  | Subtraction
  | Times
  deriving (Enum)

instance Show BinaryOperator where
  show Addition = "+"
  show BinaryDie = "d"
  show BinaryDieWithAdvantage = ">d"
  show BinaryDieWithDisadvantage = "<d"
  show Multiplication = "*"
  show Subtraction = "-"
  show Times = "x"

instance DieOp BinaryOperator where
  isDieOp BinaryDie = True
  isDieOp BinaryDieWithAdvantage = True
  isDieOp BinaryDieWithDisadvantage = True
  isDieOp _ = False


data UnaryOperator
  = Negation
  | UnaryDie
  | UnaryDieWithAdvantage
  | UnaryDieWithDisadvantage
  deriving (Enum)

instance Show UnaryOperator where
  show Negation = show Subtraction
  show UnaryDie = show BinaryDie
  show UnaryDieWithAdvantage = show BinaryDieWithAdvantage
  show UnaryDieWithDisadvantage = show BinaryDieWithDisadvantage

instance DieOp UnaryOperator where
  isDieOp UnaryDie = True
  isDieOp UnaryDieWithAdvantage = True
  isDieOp UnaryDieWithDisadvantage = True
  isDieOp _ = False


data Expression w
  = Binary BinaryOperator (w (Expression w)) (w (Expression w))
  | Unary UnaryOperator (w (Expression w))
  | Expression (w (Expression w))
  | Term Constant


isTerm (Term _) = True
isTerm _ = False


type IdentityExpression = Identity (Expression Identity)
