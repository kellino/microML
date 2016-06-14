module Syntax where

type ConstructorName = String
type VarName = String

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
                           
data Expr   = Var  VarName
               | Con  ConstructorName
               | App  Expr  Expr 
               | Lam  [Pat] Expr 
               | Def Expr Expr 
               | StringLit String
               | Char Char
               | PrimBinOp  PrimitiveOp Expr  Expr 
               | IfThenElse  Expr  Expr  Expr 
               | Num Integer
               | Double Double
               | Boolean  Bool
               | Neg Expr
               | Not  Expr 
               deriving Show

data Pat  = PVar  VarName
             | PApp ConstructorName [Pat]
             | Wildcard 
             | IntPat  Int
             | BoolPat  Bool
             deriving Show
