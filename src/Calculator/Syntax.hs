module Calculator.Syntax
  ( Expr(..)
  , Op(..)
  )
  where

data Expr
  = Double Double
  | Calc Op Expr Expr
  | Var String
  | Subst Expr Expr
  | Error String
  deriving (Eq, Show)

data Op
  = Times
  | Divide
  | Plus
  | Minus
  deriving (Eq, Show)

