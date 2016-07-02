module MicroML.Syntax where

import qualified Data.Text.Lazy as L
import qualified Data.Map as Map

type TermEnv = Map.Map String Expr

type Name = String
type VarName = String
type ConName = String

data Expr
  = Var Name
  | Constructor Name
-- | Case Expr [Expr]
  | App Expr Expr
  | Lam Name Expr
  | Let Name Expr Expr
  | Lit Lit
  | List [Expr]
  | If Expr Expr Expr
  | FixPoint Expr
  | Op Binop Expr Expr
  | UnaryMinus Expr
  | MLError String
  | ListComp Expr Expr Expr
  | Closure Name Expr TermEnv
  deriving (Eq, Ord)

instance Show Expr where
    show (Lit (LInt n))      = show n
    show (Lit (LDouble d))   = show d
    show (Lit (LBoolean b))  = show b
    show (Lit (LString str)) = show str
    show (Lit (LChar c))     = show c
    show (Var x)             = show x
    show Closure{}           = "<<closure>>"
    show (Lam n e)           = show n ++ show e

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

data MLError
    = Default L.Text
    deriving (Eq, Ord)

instance Show MLError where
    show (Default msg) = show msg

data Program = Program [Decl] Expr deriving Eq

type Decl = (String, Expr)
