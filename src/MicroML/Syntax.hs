module MicroML.Syntax where

import qualified Data.Map as Map

--------------------
-- TYPES SYNONYMS --
--------------------

type TermEnv = Map.Map String Expr
type Name = String
type VarName = String
type ConName = String
type Decl = (String, Expr)
type ErrorMsg = String

----------
-- ADTs --
----------

data Program = Program [Decl] Expr deriving Eq

data Expr
  = Var Name
  | Constructor Name
  | App Expr Expr
  | Lam Name Expr
  | Let Name Expr Expr
  | Lit Lit
  | If Expr Expr Expr
  | FixPoint Expr
  | BinOp Binop Expr Expr
  | UnaryOp UnaryOp Expr
  | Closure Name Expr TermEnv
  | PrimitiveErr MLError
  | Nil
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
     Chr | Ord  | Read | Show
   | Car | Cdr 
   | Not | Minus | OpLog -- unary maths ops
    deriving (Eq, Ord)

instance Show UnaryOp where
    show Car   = red ++ "Car" ++ clear
    show Cdr   = red ++ "Cdr" ++ clear
    show Minus = red ++ "negative" ++ clear
    show OpLog = red ++ "log" ++ clear
    show Show  = red ++ "show" ++ clear
    show Read  = red ++ "read" ++ clear
    show Chr   = red ++ "chr" ++ clear
    show Ord   = red ++ "ord" ++ clear

data Binop = 
        OpAdd | OpSub | OpMul | OpDiv | OpIntDiv | OpExp | OpMod 
      | OpOr | OpXor | OpAnd | OpEq | OpLe | OpLt | OpGe 
      | OpGt | OpNotEq | OpCons | OpAppend | OpPipe
      deriving (Eq, Ord)      

instance Show Binop where
    show OpAdd    = red ++ "addition" ++ clear
    show OpSub    = red ++ "subtraction" ++ clear
    show OpMul    = red ++ "multiplication" ++ clear
    show OpDiv    = red ++ "division" ++ clear
    show OpIntDiv = red ++ "integer division" ++ clear
    show OpExp    = red ++ "exponents" ++ clear
    show OpMod    = red ++ "modulo" ++ clear
    show OpOr     = red ++ "inclusive or" ++ clear
    show OpXor    = red ++ "exclusive or" ++ clear
    show OpAnd    = red ++ "logical and" ++ clear
    show OpEq     = red ++ "equals" ++ clear
    show OpLe     = red ++ "less than or equal to" ++ clear
    show OpLt     = red ++ "less than" ++ clear
    show OpGe     = red ++ "greater than or equal to" ++ clear
    show OpGt     = red ++ "greater than" ++ clear
    show OpNotEq  = red ++ "not equal to" ++ clear
    show OpCons   = red ++ "Cons" ++ clear
    show OpAppend = red ++ "concatenation" ++ clear
    show OpPipe   = red ++ "pipe" ++ clear

data MLError
    = MathsPrim String 
    | ListPrim String
    | CharPrim String
    deriving (Eq, Ord)

instance Show MLError where
    show (MathsPrim str) = show str
    show (ListPrim str)  = show str
    show (CharPrim str)  = show str

-----------------
-- HELPER DEFS --
-----------------

red, clear, bold :: String
red = "\ESC[31m"
clear = "\ESC[0m"
bold = "\ESC[1m"
