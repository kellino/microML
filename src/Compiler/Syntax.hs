module Compiler.Syntax where

import Language.C.DSL

data CPP = 
        Decl CDecl
      | CPPExp CExpr
      | CppFunc CFunDef

