module Language.Syntax where

import qualified Data.Text as T

type Name = String
type VarName = String
type ConName = String

data Expr
  = Var Name
  | Constructor Name
  | Case Expr [Expr]
  | App Expr Expr
  | Lam Name Expr
  | Let Name Expr Expr
  | Lit Lit
  | List [Expr]
  | If Expr Expr Expr
  | FixPoint Expr
  | Op Binop Expr Expr
  | UnaryMinus Expr
  | Pat Pat
  deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LDouble Double
  | LBoolean Bool
  | LString String
  | LChar Char

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
      | OpNotEq
      deriving (Eq, Ord, Show)      

data Pat
    = Wildcard
  | PApp String [Expr]
  | PVar VarName
  | PCon ConName
  | PInt Integer
  | PDouble Double
  | PBool Bool
  deriving (Eq, Ord, Show)

data MLError
    = Default T.Text

instance Show MLError where
    show (Default msg) = show msg


data Program = Program [Decl] Expr deriving Eq

type Decl = (String, Expr)
