module LogicExpressions where

  data Expr a =
    Var a |
    Not (Expr a) |
    And (Expr a) (Expr a) |
    Or (Expr a) (Expr a) |
    Xor (Expr a) (Expr a)
    deriving (Eq)

  instance Show (a) => Show (Expr a) where
    show (Var a) = show a
    show (Not a) =  "Not " ++ show a
    show (And a b) = "(" ++ show a ++ ") And (" ++ show b ++ ")"
    show (Or a b) = "(" ++ show a ++ ") Or (" ++ show b ++ ")"
    show (Xor a b) = "(" ++ show a ++ ") Xor (" ++ show b ++ ")"

  type LogicExpr = Expr Bool

  eval :: LogicExpr -> Bool
  eval (Var True) = True
  eval (Var False) = False
  eval (Not a) = evalNot (eval a)
  eval (And a b) = evalAnd (eval a) (eval b)
  eval (Or a b ) = evalOr (eval a) (eval b)
  eval (Xor a b ) = evalXor (eval a) (eval b)

  evalNot :: Bool -> Bool
  evalNot True = False
  evalNot False = True

  evalAnd :: Bool -> Bool -> Bool
  evalAnd True True = True
  evalAnd _ _ = False

  evalOr :: Bool -> Bool -> Bool
  evalOr False False = False
  evalOr _ _ = True

  evalXor :: Bool -> Bool -> Bool
  evalXor True True = False
  evalXor False False = False
  evalXor _ _ = True
