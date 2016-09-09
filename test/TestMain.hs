module Main where

import ListPrimitivesSpec
import MathsPrimitives
import EvalSpec
import CallGraphSpec

main :: IO ()
main = do
    listprims
    mathsprims
    evalspec
    callGraph
