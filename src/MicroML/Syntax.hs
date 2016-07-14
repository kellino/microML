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
  | UnaryOp UnaryOp Expr
  | ListComp Expr Expr Expr
  | Closure Name Expr TermEnv
  | PrimitiveErr MLError
  deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LDouble Double
  | LBoolean Bool
  | LString String
  | LChar Char
  | LTup [Expr]

  deriving (Show, Eq, Ord)

data UnaryOp =
        Car | Cdr | Not
     | Minus | OpLog -- unary maths ops
    deriving (Show, Eq, Ord)

data Binop = 
        OpAdd | OpSub | OpMul | OpDiv | OpIntDiv | OpExp | OpMod 
      | OpOr | OpXor | OpAnd | OpEq | OpLe | OpLt | OpGe 
      | OpGt | OpNotEq | OpCons | OpComp | OpAppend
      deriving (Eq, Ord)      

instance Show Binop where
    show OpAdd    = red ++ "addition" ++ unred
    show OpSub    = red ++ "subtraction" ++ unred
    show OpMul    = red ++ "multiplication" ++ unred
    show OpDiv    = red ++ "division" ++ unred
    show OpIntDiv = red ++ "integer division" ++ unred
    show OpExp    = red ++ "exponents" ++ unred
    show OpMod    = red ++ "modulo" ++ unred
    show OpOr     = red ++ "inclusive or" ++ unred
    show OpXor    = red ++ "exclusive or" ++ unred
    show OpAnd    = red ++ "logical and" ++ unred
    show OpEq     = red ++ "equals" ++ unred
    show OpLe     = red ++ "less than or equal to" ++ unred
    show OpLt     = red ++ "less than" ++ unred
    show OpGe     = red ++ "greater than or equal to" ++ unred
    show OpGt     = red ++ "greater than" ++ unred
    show OpNotEq  = red ++ "not equal to" ++ unred
    show OpCons   = red ++ "cons" ++ unred
    show OpComp   = red ++ "composition" ++ unred
    show OpAppend = red ++ "concatenation" ++ unred

red, unred :: String
red = "\ESC[31m"
unred = "\ESC[0m"

data MLError
    = MathsPrim String 
    | ListPrim String
    deriving (Eq, Ord)

instance Show MLError where
    show (MathsPrim str) = show str
    show (ListPrim str)  = show str

data Program = Program [Decl] Expr deriving Eq

type Decl = (String, Expr)
