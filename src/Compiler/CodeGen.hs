{-# LANGUAGE OverloadedStrings #-}

module Compiler.CodeGen where

import Compiler.MicroBitHeader
--import Compiler.State
import MicroML.Syntax
import MicroML.Parser

import Language.C.DSL
import qualified Data.Text.Lazy as L
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

hoistError :: Show a => Either a t -> t
hoistError (Right val) = val
hoistError (Left err) = error $ show err

eval :: (String, Expr) -> CExpr
eval (name, res) = 
    case res of
      (Lit (LString st)) -> str st
      Var x -> fromMaybe (error "Unbound variable error") $ Map.lookup x microBitAPI
      App a b -> do
          let func = eval (name, a)
          let arg = eval (name, b)
          func # [arg]

fromFile :: L.Text -> [(String, Expr)]
fromFile source = hoistError $ parseProgram "<from file>" source

mainFunc :: CExpr -> CFunDef
mainFunc func  = 
    fun [intTy] "main" [] $ hBlock [
        microBitInit
      , func
      , releaseFiber
    ]

compile :: L.Text -> L.Text -> IO ()
compile file newFile = do
    let cFile = L.unpack newFile ++ ".cpp"
    writeFile cFile $ show . fromFile $ file
    --writeFile cFile $ microBitIncludes ++ (show . pretty $ mainFunc (eval (head $ fromFile file)))
