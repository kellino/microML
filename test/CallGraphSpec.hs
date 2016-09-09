module CallGraphSpec where

import Test.Hspec
import MicroML.Syntax
import Compiler.CallGraph
import Control.Exception (evaluate)

callGraph :: IO ()
callGraph = hspec $
    describe "callGraph" $ do
        describe "checkForDuplicates" $ do
            it "should do nothing to a list with no duplicates" $
                checkForDuplicates [("test1", Lit (LInt 1)), ("test2", Lit (LInt 1))] `shouldBe` [("test1", Lit (LInt 1)), ("test2", Lit (LInt 1))] 
            it "should throw an error if there are duplicates" $
                evaluate (checkForDuplicates [("test", Lit (LInt 1)), ("test", Lit (LInt 1))]) `shouldThrow` anyException 


        describe "reachableFromMain" $ do
            it "should throw an error if not every function is reachable" $
                evaluate (reachableFromMain [("main", Var "single"), ("double", Lit (LInt 1))]) `shouldThrow` anyException
            it "should return the list unchanged if it's fully connected from main" $
                reachableFromMain [("main", Var "double"), ("double", Lit (LInt 1))] `shouldBe` [("main", Var "double"), ("double", Lit (LInt 1))]


