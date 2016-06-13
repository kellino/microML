module Syntax where

-------------------
-- Type Synonyms --
-------------------

type ConstructorName = String
type VarName = String


----------------
-- Data Types --
----------------


data PrimitiveOp  = OpAdd
                  | OpSub
                  | OpMul
                  | OpDiv
                  | OpMod
                  | OpOr
                  | OpAnd
                  | OpEq
                  | OpNe
                  | OpLe
                  | OpLt
                  | OpGe
                  | OpGt
                  deriving Show                           
                           
data Expr   = Var VarName
               | Con ConstructorName
               | StringLit String
               | App  Expr  Expr 
               | Lam  Pat  Expr 
             -- | Let  [Def ] Expr 
               | PrimBinOp  PrimitiveOp Expr  Expr 
               | IfThenElse  Expr  Expr  Expr 
               | Num  Integer
               | Boolean  Bool
               | UnaryMinus  Expr 
               | Not  Expr 
               deriving Show


data Pat  = PVar  VarName
             | PApp  ConstructorName Pat 
             | Wildcard 
             | IntPat  Int
             | BoolPat  Bool
             deriving Show
