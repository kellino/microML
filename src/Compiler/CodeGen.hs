{-# LANGUAGE OverloadedStrings #-}

module Compiler.CodeGen where

import Compiler.MicroBitHeader
import MicroML.Syntax
import MicroML.Parser

import Language.C.DSL
import qualified Data.Text.Lazy as L
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)


hoistError :: Show a => Either a t -> t
hoistError (Right val) = val
hoistError (Left err) = error $ show err

eval :: (t, Expr) -> CExpr
eval (_, res) = 
    case res of
      (Lit (LString st)) -> str st
      Var x -> fromMaybe (error "") $ Map.lookup x microBitAPI
      App a b -> do
          let func = eval ("", a)
          let arg = eval ("", b)
          func # [arg]

fromFile :: L.Text -> [(String, Expr)]
fromFile source = hoistError $ parseProgram "<from file>" source

mainFunc :: CExpr -> CFunDef
mainFunc func  = 
    fun [intTy] "main" [] $ hBlock [
        "uBit.init()", func, releaseFiber
    ]

compile :: L.Text -> IO ()
compile file = do
    let cFile = "out.cpp"
    writeFile cFile $ microBitIncludes ++ (show . pretty $ mainFunc (eval (head $ fromFile file)))
