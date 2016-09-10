module MathsPrimitives where

import Test.Hspec
import MicroML.ListPrimitives
import MicroML.MathsPrimitives
import MicroML.Syntax
import Control.Exception (evaluate)

import Repl.Eval

mathsprims :: IO ()
mathsprims = hspec $
    describe "mathsprims" $ do
        describe "add" $ do
            it "should add two integers" $
                add (Lit (LInt 1)) (Lit (LInt 1)) `shouldBe` Lit (LInt 2)
            it "should add two doubles" $
                add (Lit (LDouble 1.1)) (Lit (LDouble 1.1)) `shouldBe` Lit (LDouble 2.2)
            it "should add an int to a double" $
                add (Lit (LInt 1)) (Lit (LDouble 1.1)) `shouldBe` Lit (LDouble 2.1)
            it "should add a double to an int" $
                add (Lit (LDouble 1.1)) (Lit (LInt 1)) `shouldBe` Lit (LDouble 2.1)

            -- test for failure, but the type checker should prevent this ever happening
            it "should fail on other input" $
                evaluate (add (Lit (LChar 'a')) (Lit (LChar 'a'))) `shouldThrow` anyException

        describe "sub" $ do
            it "should sub two integers" $
                sub (Lit (LInt 1)) (Lit (LInt 1)) `shouldBe` Lit (LInt 0)
            it "should sub two doubles" $
                sub (Lit (LDouble 1.1)) (Lit (LDouble 1.1)) `shouldBe` Lit (LDouble 0.0)
            it "should sub an int and a double" $
                sub (Lit (LInt 2)) (Lit (LDouble 1.1)) `shouldBe` Lit (LDouble 0.9)
            it "should sub a double and an int" $
                sub (Lit (LDouble 1.1)) (Lit (LInt 1)) `shouldBe` Lit (LDouble 0.1)

            -- test for failure, but the type checker should prevent this ever happening
            it "should fail on other input" $
                sub (Lit (LChar 'a')) (Lit (LChar 'a')) `shouldBe` PrimitiveErr (MathsPrim "perhaps you meant (-)")
    
        describe "mul" $ do
            it "should mul two integers" $
                mul (Lit (LInt 1)) (Lit (LInt 1)) `shouldBe` Lit (LInt 1)
            it "should mul two doubles" $
                mul (Lit (LDouble 1.1)) (Lit (LDouble 1.1)) `shouldBe` Lit (LDouble 1.21)
            it "should mul an int and a double" $
                mul (Lit (LInt 1)) (Lit (LDouble 1.1)) `shouldBe` Lit (LDouble 1.1)
            it "should mul a double and an int" $
                mul (Lit (LDouble 1.1)) (Lit (LInt 1)) `shouldBe` Lit (LDouble 1.1)

        describe "truncate'" $ do
            it "should remove trailing zeros" $
                truncate' 1.00000002 `shouldBe` 1.0
            it "should round repeating decimal" $
                truncate' 0.8999999 `shouldBe` 0.9
            it "should round repeating decimal with leading number" $
                truncate' 1.2999999 `shouldBe` 1.3
            it "should not round 1.299993" $
                truncate' 1.299993 `shouldBe` 1.299993
