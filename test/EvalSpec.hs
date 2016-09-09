module EvalSpec where

import Test.Hspec
import Repl.Eval
import MicroML.Syntax
import Control.Exception (evaluate)

evalspec :: IO ()
evalspec = hspec $
    describe "evalspec" $ do
        describe "eval" $ do
            it "should eval an int to an int" $
                eval emptyTmenv (Lit (LInt 1)) `shouldBe` Lit (LInt 1)
            it "should eval a double to a double" $
                eval emptyTmenv (Lit (LDouble 1.0)) `shouldBe` Lit (LDouble 1.0)
            it "should eval a char to a char" $
                eval emptyTmenv (Lit (LChar 'a')) `shouldBe` Lit (LChar 'a')
            it "should eval a string to a string" $
                eval emptyTmenv (Lit (LString "hello")) `shouldBe` Lit (LString "hello")
            it "should eval a bool to a bool" $
                eval emptyTmenv (Lit (LBoolean True)) `shouldBe` Lit (LBoolean True)
            it "should eval Nil to Nil" $
                eval emptyTmenv Nil `shouldBe` Nil
