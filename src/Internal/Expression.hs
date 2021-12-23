module Internal.Expression where


type Constant = Word


data BinaryOperator
  = Addition
  | BinaryDie
  | BinaryDieWithAdvantage
  | BinaryDieWithDisadvantage
  | Multiplication
  | Subtraction
  deriving (Enum)

instance Show BinaryOperator where
  show Addition = "+"
  show BinaryDie = "d"
  show BinaryDieWithAdvantage = ">d"
  show BinaryDieWithDisadvantage = "<d"
  show Multiplication = "*"
  show Subtraction = "-"


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


data Expression
  = Binary BinaryOperator Expression Expression
  | Unary UnaryOperator Expression
  | Expression Expression
  | Term Constant

instance Show Expression where
  show (Binary op l r) = concat [show l, " ", show op, " ", show r]
  show (Unary op e) = show op ++ show e
  show (Expression e) = concat ["(", show e, ")"]
  show (Term const) = show const
