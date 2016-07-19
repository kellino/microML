{-# LANGUAGE OverloadedStrings #-}

module Compiler.CodeGen where

import Compiler.MicroBitHeader
import Compiler.Syntax
import MicroML.Syntax
import MicroML.Parser

import Language.C.DSL 
import qualified Data.Text.Lazy as L
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

hoistError :: Show a => Either a t -> t
hoistError (Right val) = val
hoistError (Left err) = error $ show err

eval :: (String, Expr) -> CPP
eval (name, res) = 
    case res of
      (Lit (LInt _))     -> Decl $ int "x" .= 1
      (Lit (LString st)) -> CPPExp $ str st
      Var x -> CPPExp $ fromMaybe (error "Unbound variable error") $ Map.lookup x microBitAPI
      App a b -> do
          let (CPPExp func) = eval (name, a)
          let (CPPExp arg) = eval (name, b)
          CPPExp $ func # [arg]
      Lam _ _ -> CppFunc makeFunction

makeFunction :: CFunDef
makeFunction =
    fun [intTy] "inc" [int "a"] $ hBlock [
        creturn ("a" + 1)                                     
    ]

fromFile :: L.Text -> [(String, Expr)]
fromFile source = hoistError $ parseProgram "<from file>" source

mainFunc :: CPP -> CFunDef
mainFunc body =
    fun [intTy] "main" [] $ block [
      intoB $ fromMaybe "" $ declarations body
    , intoB microBitInit                               
    , intoB $ fromMaybe blank $ expressions body
    , intoB releaseFiber
    ]

declarations :: CPP -> Maybe CDecl
declarations dec = 
    case dec of
      (Decl xs) -> Just xs
      _         -> Nothing
    
expressions :: CPP -> Maybe CExpr
expressions ex = 
    case ex of
      (CPPExp ys) -> Just ys
      _           -> Nothing

-- another hack right here!
removeBlanks :: String -> String
removeBlanks = unlines . filter (\x -> L.strip (L.pack x) /= ";") . lines

renderC :: CFunDef -> String
renderC = show . pretty

compile :: L.Text -> L.Text -> IO ()
compile file newFile = do
    let cFile = L.unpack newFile ++ ".cpp"
    --writeFile cFile $ show . fromFile $ file
    writeFile cFile $ microBitIncludes ++ removeBlanks (renderC makeFunction ++ filecontents file)
        where filecontents f = renderC $ mainFunc (eval (head $ fromFile f))
