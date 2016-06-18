-- very liberally borrowing from the github repo metafun by gergoerdi, but
-- with a few simplifications and additions
-- https://github.com/gergoerdi/metafun/tree/master/src


module Syntax where

type ConstructorName = String
type VarName = String
type Dataname = String
type ADTParam = String

data TypePrimitive = TypeInt | TypeDouble | TypeBool deriving (Eq, Ord, Show)

data TypeDec = TypeAlias String String String deriving (Eq, Ord, Show)

data Type = TypeVar String
          | TypeFunc Type [Type]
          | TypeApp Type Type
          | TypeData String
          | TypeList Type
          | TypeCurry [Type]
          | TypePrimitive TypePrimitive
          deriving (Eq, Ord, Show)

data PrimitiveOp  = OpAdd
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
                  deriving (Eq, Ord, Show)                          
                           
data Expr = Var VarName
          | Con  ConstructorName
          | App  Expr  Expr 
          | Lam  [Pat] Expr 
          | Def Expr Expr 
          | StringLit String
          | Char Char
          | PrimBinOp PrimitiveOp Expr  Expr 
          | IfThenElse Expr Expr Expr 
          | Number Integer
          | Double Double
          | Boolean  Bool
          | Neg Expr
          | Not Expr 
          | TypeSig Type [Type]
          | Tuple [Expr] 
          | ADT Expr Expr [Expr]
          | DataCon Expr Expr
          | TypeDec TypeDec
          deriving (Eq, Ord, Show)

data Pat = PVar VarName
         | PApp ConstructorName [Pat]
         | Wildcard 
         | IntPat  Int
         | BoolPat  Bool
         deriving (Eq, Ord, Show)
