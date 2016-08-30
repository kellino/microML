module Repl.ParseTree (showTree) where

import Data.Tree
import Data.Tree.Pretty

import MicroML.Syntax

-- | pretty print the parse tree of an expression in the repl

exprToTree :: (String, Expr) -> Tree String
exprToTree (nm, ex) =
    Node nm $ etoT ex

etoT :: Expr -> [Tree Name]
etoT (FixPoint e1) = [Node "rec" (etoT e1)]
etoT Nil     = [Node "[]" []] 
etoT (App e1 e2) = etoT e1 ++ etoT e2
etoT (UnaryOp op e1) = [Node (ppUnop op) (etoT e1)]
etoT (If cond tr fls) = [Node "if" (etoT cond ++ etoT tr ++ etoT fls)]
etoT (Var x) = [Node x []]
etoT (Let nm e1 e2) = [Node nm (etoT e1 ++ etoT e2)]
etoT (Lit (LInt x)) = [Node (show x) []]
etoT (Lit (LDouble x)) = [Node (show x) []]
etoT (Lit (LChar x)) = [Node (show x) []]
etoT (Lit (LString x)) = [Node x []]
etoT (Lit (LBoolean x)) = [Node (show x) []]
etoT (Lit (LTup x)) = [Node (show x) []]
etoT (PrimitiveErr _) = [Node "error" []]
etoT (Lam nm ex) = [Node nm $ etoT ex]
etoT (BinOp op e1 e2) = [Node (pp op) (etoT e1 ++ etoT e2)] 
etoT x = [Node (show x) []]

ppUnop :: UnaryOp -> String
ppUnop OpLog = "log"
ppUnop Car = "head"
ppUnop Cdr = "tail"
ppUnop Read = "read"
ppUnop Show = "show"
ppUnop Not = "not"
ppUnop Minus = "-"
ppUnop Chr = "char"
ppUnop Ord = "ord"

pp :: Binop -> String
pp OpAdd = "+"
pp OpMul = "*"
pp OpEq  = "=="
pp OpAppend = "++"
pp OpNotEq = "≠"
pp OpOr  = "or"
pp OpExp = "^"
pp OpDiv  = "÷"
pp OpIntDiv = "÷"
pp OpPipe = ">>"
pp OpMod = "%"
pp OpAnd = "and"
pp OpXor = "xor"
pp OpSub = "-"
pp OpLe = "<="
pp OpLt = "<"
pp OpGe = ">="
pp OpGt = ">"
pp OpCons = ":"

showTree :: (String, Expr) -> IO ()
showTree tr = putStrLn $ drawVerticalTreeWith 5 (exprToTree tr)
