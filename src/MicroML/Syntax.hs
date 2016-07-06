module MicroML.Syntax where

import qualified Data.Map as Map

type TermEnv = Map.Map String Expr

type Name = String
type VarName = String
type ConName = String

data Expr
  = Var Name
  | Constructor Name
  | App Expr Expr
  | Lam Name Expr
  | Let Name Expr Expr
  | Lit Lit
  | List [Expr]
  | If Expr Expr Expr
  | FixPoint Expr
  | Op Binop Expr Expr
  | ListOp ListOp Expr
  | UnaryMinus Expr
  | ListComp Expr Expr Expr
  | Closure Name Expr TermEnv
  deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LDouble Double
  | LBoolean Bool
  | LString String
  | LChar Char

  deriving (Show, Eq, Ord)

data ListOp =
        Car 
      | Cdr
    deriving (Show, Eq, Ord)

data Binop = 
        OpAdd | OpSub | OpMul | OpDiv | OpMod | OpExp | OpOr | OpXor
      | OpAnd | OpEq | OpLe | OpLt | OpGe | OpGt | OpNotEq
      | OpCons | OpComp | OpAppend
      deriving (Eq, Ord, Show)      

data MLError
    = Default String 
    deriving (Show, Eq, Ord)

data Program = Program [Decl] Expr deriving Eq

type Decl = (String, Expr)
