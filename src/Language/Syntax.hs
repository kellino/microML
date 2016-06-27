{-# LANGUAGE OverloadedStrings #-}

module Language.Syntax where

type Name = String

data Expr
  = Var Name
  | Constructor Name
  | App Expr Expr
  | Lam Name Expr
  | Let Name Expr Expr
  | FixPoint Expr
  | Lit Lit
  | Pat Pat
  | UnaryMinus Expr
  | List [Expr]
  | If Expr Expr Expr
  | Op Binop Expr Expr
  | Case Expr [Match]
  deriving (Show, Eq, Ord)

data Lit
  = Number Integer
  | Boolean Bool
  | Char Char
  | Double Double
  | String String
  deriving (Show, Eq, Ord)

data Match = Match 
    { _matchName :: Name
      , _matchPats :: [Match]
    } deriving (Eq, Show, Ord)
    
data Pat =
        PVar Name
      | PApp Name [Pat]
      | PLit Lit
      | PWild
      | PBool Bool
      | PNum Integer
      | PDouble Double
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
