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
    show Car   = "Car" 
    show Cdr   = "Cdr"
    show Minus = "negative" 
    show OpLog = "log" 
    show Show  = "show" 
    show Read  = "read"
    show Chr   = "chr"
    show Ord   = "ord" 
    show Not   = "not"

data Binop = 
        OpAdd | OpSub | OpMul | OpDiv | OpIntDiv | OpExp | OpMod 
      | OpOr | OpXor | OpAnd | OpEq | OpLe | OpLt | OpGe 
      | OpGt | OpNotEq | OpCons | OpAppend | OpPipe | OpEnum
      deriving (Eq, Ord)      

instance Show Binop where
    show OpAdd    =  "addition" 
    show OpSub    =  "subtraction" 
    show OpMul    =  "multiplication" 
    show OpDiv    =  "division" 
    show OpIntDiv =  "integer division" 
    show OpExp    =  "exponent" 
    show OpMod    =  "modulo" 
    show OpOr     =  "inclusive or" 
    show OpXor    =  "exclusive or" 
    show OpAnd    =  "logical and" 
    show OpEq     =  "equals" 
    show OpLe     =  "less than or equal to" 
    show OpLt     =  "less than" 
    show OpGe     =  "greater than or equal to" 
    show OpGt     =  "greater than" 
    show OpNotEq  =  "not equal to" 
    show OpCons   =  "Cons" 
    show OpAppend =  "concatenation" 
    show OpPipe   =  "pipe" 
    show OpEnum   =  "enum" 

data MLError
    = MathsPrim String 
    | ListPrim String
    | CharPrim String
    deriving (Eq, Ord)

instance Show MLError where
    show (MathsPrim str) = str
    show (ListPrim str)  = str
    show (CharPrim str)  = str

-----------------
-- HELPER DEFS --
-----------------

red, clear, bold :: String
red = "\ESC[31m"
clear = "\ESC[0m"
bold = "\ESC[1m"
