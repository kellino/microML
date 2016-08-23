module ListPrimitivesTest where

import MicroML.ListPrimitives
import MicroML.Syntax
import Test.Hspec


listprims :: IO ()
listprims = hspec $ 
    describe "listprims" $ do
        describe "car" $ do
            it "gets the head of a list" $ 
                car (BinOp OpCons (Lit (LInt 3)) Nil) `shouldBe` (Lit (LInt 3) :: Expr)
            it "gets the head of a string" $
                car (Lit (LString "hello")) `shouldBe` (Lit (LChar 'h') :: Expr)

        describe "cdr" $ do
            it "gets the tail of a string" $
                cdr (BinOp OpCons (Lit (LInt 1)) (BinOp OpCons (Lit (LInt 2)) Nil)) `shouldBe` ((BinOp OpCons (Lit (LInt 2)) Nil) :: Expr)
            it "gets the tail of a string" $
                cdr (Lit (LString "hello")) `shouldBe` (Lit (LString "ello") :: Expr)
