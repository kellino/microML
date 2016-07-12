module MicroML.MathsPrimitives where

import MicroML.Syntax

sqrt' :: Expr -> Expr
sqrt' a = do
    let res = sqRoot a
    case res of
      Just val@(Lit (LDouble x)) -> 
          if isNaN x 
             then PrimitiveErr . MathsPrim $ "the square root of a negative number is undefined"
             else val
      Nothing -> PrimitiveErr . MathsPrim $ "you didn't enter a number"

sqRoot :: Expr -> Maybe Expr
sqRoot (Lit (LInt a)) = Just $ Lit . LDouble $ sqrt $ realToFrac a
sqRoot (Lit (LDouble a)) = Just $ Lit . LDouble $ sqrt a
sqRoot _ = Nothing 

-- this doesn't work
log' :: Expr -> Double -> Expr
log' (Lit (LInt a)) n = Lit . LDouble $ logBase n $ realToFrac a
