{-# LANGUAGE OverloadedStrings #-}

module Compiler.PrettyCPP where

import MicroML.Syntax
import Text.PrettyPrint

showText :: Expr -> Doc
showText = text . show

semiWithNewLine :: Doc
semiWithNewLine = semi <> "\n"

parensWithSemi :: Doc -> Doc
parensWithSemi d = parens d <> semi <> "\n"

bracesNewLine :: Doc -> Doc
bracesNewLine d = braces ("\n\t" <> d) <> "\n"

class Pretty p where
    ppr :: p -> Doc

instance Pretty Binop where 
    ppr OpAdd    = " + "
    ppr OpSub    = " - "
    ppr OpMul    = " * "
    ppr OpDiv    = " / "
    ppr OpIntDiv = " / "
    ppr OpMod    = " % "
    ppr OpOr     = " || "
    ppr OpAnd    = " && "
    ppr OpNotEq  = " != "
    ppr OpEq     = " == "
    ppr OpExp    = "^"
    ppr OpLe     = " <= "
    ppr OpLt     = " < "
    ppr OpGe     = " >= "
    ppr OpGt     = " > "
    ppr OpXor    = undefined
    ppr OpAppend = undefined
