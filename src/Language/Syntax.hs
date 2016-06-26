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
  | UnaryMinus Expr
  | List List 
  | If Expr Expr Expr
  | Op Binop Expr Expr
  deriving (Show, Eq, Ord)

data List = Nil | Cons Expr List deriving (Show, Ord, Eq) 

data Lit
  = Number Integer
  | Boolean Bool
  | Char Char
  | Double Double
  | String String
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

data Type = 
        TVar TVar
      | TCon TyCon
      | TApp Type Type
      deriving (Show, Eq, Ord)

data TVar = TV { tvName :: Name } deriving (Show, Eq, Ord)
data TyCon = AlgTyCon { tyId :: Name } | PrimTyCon { tyId :: Name } deriving (Show, Eq, Ord)

data Program = Program [Decl] Expr deriving Eq

type Decl = (String, Expr)
