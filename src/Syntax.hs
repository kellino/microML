module Syntax where

type Name = String

data Expr
  = Var Name
  | Constructor Name
  | App Expr Expr
  | Lam Name Expr
  | Let Name Expr Expr
  | Lit Lit
  | UnaryMinus Expr
  | List [Expr]
  | If Expr Expr Expr
  | Op Binop Expr Expr
  deriving (Show, Eq, Ord)

data Lit
  = Number Integer
  | Boolean Bool
  | Char Char
  | Double Double
  | String String
  deriving (Show, Eq, Ord)

data Binop = 
        OpAdd
      | OpSub
      | OpMul
      | OpDiv
      | OpMod
      | OpExp
      | OpOr
      | OpAnd
      | OpEq
      | OpNe
      | OpLe
      | OpLt
      | OpGe
      | OpGt
      | OpCom
      deriving (Eq, Ord, Show)         

data Program = Program [Decl] Expr deriving Eq

type Decl = (String, Expr)
