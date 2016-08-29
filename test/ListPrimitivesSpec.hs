module ListPrimitivesSpec where

import Test.Hspec
import MicroML.ListPrimitives
import MicroML.Syntax
import Control.Exception (evaluate)


listprims :: IO ()
listprims = hspec $ 
    describe "listprims" $ do
        describe "car" $ do
            it "gets the head of a list" $ 
                car (BinOp OpCons (Lit (LInt 3)) Nil) `shouldBe` (Lit (LInt 3) :: Expr)
            it "gets the head of a string" $
                car (Lit (LString "hello")) `shouldBe` (Lit (LChar 'h') :: Expr)
            it "should fail on non-list ints" $ 
                evaluate (car (Lit (LInt 1)))        `shouldThrow` anyException
            it "should fail on non-list doubles" $ 
                evaluate (car (Lit (LDouble 1)))     `shouldThrow` anyException
            it "should fail on non-list chars" $ 
                evaluate (car (Lit (LChar 'a')))     `shouldThrow` anyException
            it "should fail on non-list bools" $ 
                evaluate (car (Lit (LBoolean True))) `shouldThrow` anyException


        describe "cdr" $ do
            it "gets the tail of a string" $
                cdr (BinOp OpCons (Lit (LInt 1)) (BinOp OpCons (Lit (LInt 2)) Nil)) `shouldBe` ((BinOp OpCons (Lit (LInt 2)) Nil) :: Expr)
            it "gets the tail of a string" $
                cdr (Lit (LString "hello")) `shouldBe` (Lit (LString "ello") :: Expr)
            it "should fail on non-list ints" $ 
                evaluate (cdr (Lit (LInt 1)))        `shouldThrow` anyException
            it "should fail on non-list doubles" $ 
                evaluate (cdr (Lit (LDouble 1)))     `shouldThrow` anyException
            it "should fail on non-list chars" $ 
                evaluate (cdr (Lit (LChar 'a')))     `shouldThrow` anyException
            it "should fail on non-list bools" $ 
                evaluate (cdr (Lit (LBoolean True))) `shouldThrow` anyException

        describe "append" $ do
            it "joins string literals" $
                append (Lit (LString "hello ")) (Lit (LString "there")) `shouldBe` Lit (LString "hello there")
            it "joins lists" $
                append (BinOp OpCons (Lit (LInt 1)) Nil) (BinOp OpCons (Lit (LInt 2)) Nil) `shouldBe` (BinOp OpCons (Lit (LInt 1)) (BinOp OpCons (Lit (LInt 2)) Nil) :: Expr)
            it "should fail on non-lists" $
                append (Lit (LInt 1)) (Lit (LInt 2)) `shouldBe` (PrimitiveErr . ListPrim $ "one of your two objects isn't a list")

        describe "show'" $ do
            it "returns a string from a string" $
                show' (Lit (LString "hello")) `shouldBe` (Lit (LString "hello"))
            it "returns a string from an int" $
                show' (Lit (LInt 1)) `shouldBe` (Lit (LString "1"))
            it "returns a string from a double" $
                show' (Lit (LDouble 2.2)) `shouldBe` (Lit (LString "2.2"))
            it "returns a string from a char" $
                show' (Lit (LChar 'c')) `shouldBe` (Lit (LString "c"))
            it "returns a string from a True boolean" $
                show' (Lit (LBoolean True)) `shouldBe` (Lit (LString "true"))
            it "returns a string from a False boolean" $
                show' (Lit (LBoolean False)) `shouldBe` (Lit (LString "false"))

        describe "read'" $ do
            it "takes a string and returns an integer" $
                read' (Lit (LString "3")) `shouldBe` (Lit (LInt 3))
            it "takes a string and returns an double" $
                read' (Lit (LString "3.3")) `shouldBe` (Lit (LDouble 3.3))
            it "should complain if passed a non-number string" $ 
                read' (Lit (LString "dog")) `shouldBe` (PrimitiveErr $ ListPrim "the string does not contain a number")

        describe "ord'" $ do
            it "takes a char and returns an int" $
                ord' (Lit (LChar 'a')) `shouldBe` (Lit (LInt 97))

        describe "chr'" $ do
            it "takes an int and returns a char" $
                chr' (Lit (LInt 97)) `shouldBe` (Lit (LChar 'a'))
