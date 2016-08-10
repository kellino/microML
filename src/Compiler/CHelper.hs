module Compiler.CHelper where

import MicroML.Syntax

import Language.C.DSL
import GHC.Float



apply :: Expr -> Expr -> CExpr
apply (Var x) (Lit (LInt y)) = str x # [int' y]
apply (Var x) (Lit (LDouble y)) = str x # [db y]


chr :: Char -> CExpr
chr = CConst . flip CCharConst undefNode . cChar

int' :: Integer -> CExpr
int' = CConst . flip CIntConst undefNode . cInteger

db :: Double -> CExpr
db d = CConst . flip CFloatConst undefNode . cFloat $ double2Float d

