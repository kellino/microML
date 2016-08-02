module Compiler.CHelper where

import Language.C.DSL
import GHC.Float

_toCExpr :: String -> CExpr
_toCExpr = str

chr :: Char -> CExpr
chr = CConst . flip CCharConst undefNode . cChar

int' :: Integer -> CExpr
int' = CConst . flip CIntConst undefNode . cInteger

db :: Double -> CExpr
db d = CConst . flip CFloatConst undefNode . cFloat $ double2Float d
