module MicroML.MathsPrimitives where

import MicroML.Syntax

sqRoot (Lit (LInt a)) = Lit . LDouble $ sqrt $ realToFrac a
sqRoot (Lit (LDouble a)) = Lit . LDouble $ sqrt a

log' :: Expr -> Double -> Expr
log' (Lit (LInt a)) n = Lit . LDouble $ logBase n $ realToFrac a
