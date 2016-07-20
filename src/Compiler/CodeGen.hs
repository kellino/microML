{-# LANGUAGE OverloadedStrings #-}

module Compiler.CodeGen where

import Compiler.MicroBitHeader
--import Compiler.Syntax
import MicroML.Syntax
import MicroML.Parser 

import Language.C.DSL 
import qualified Data.Text.Lazy as L
{-import qualified Data.Map as Map-}
import Data.Maybe (mapMaybe)
import Data.String (fromString)

hoistError :: Show a => Either a t -> t
hoistError (Right vl) = vl
hoistError (Left err) = error $ show err

compileMicroML :: (String, Expr) -> CExtDecl
compileMicroML (nm, expr) = 
    case expr of
      Lit (LInt n) -> export $ int (fromString nm) .= fromInteger n

{-eval :: (String, Expr) -> CPP-}
{-eval (name, res) = -}
    {-case res of-}
      {-(Lit (LInt _))     -> Decl $ int "x" .= 1-}
      {-(Lit (LString st)) -> CPPExp $ str st-}
      {-Var x -> CPPExp $ fromMaybe (error "Unbound variable error") $ Map.lookup x microBitAPI-}
      {-App a b -> do-}
          {-let (CPPExp func) = eval (name, a)-}
          {-let (CPPExp arg) = eval (name, b)-}
          {-CPPExp $ func # [arg]-}

makeMain :: [CExtDecl] -> [(CDecl, Maybe String, CExpr)] -> [CExtDecl]
makeMain decls inits =
    mapMaybe makeFunProtos decls
    ++ map makeProts inits
    ++ [export $ fun [intTy] "main" [] (makeBlock inits)]
        where makeBlock = hBlock . concatMap buildExp
              buildExp (_, v, ex) = maybe [ex] ((:[]) . (<-- ex) . fromString) v
              makeProts = export . (\(a, _, _) -> a)

makeFunProtos :: CExtDecl -> Maybe CExtDecl
makeFunProtos (CFDefExt (CFunDef specs declr _ _ a)) =
    Just . export $ CDecl specs [(Just declr, Nothing, Nothing)] a
makeFunProtos _ = Nothing

renderC :: [CExtDecl] -> String
renderC = concatMap $ show . pretty

writeCFile :: L.Text -> [CExtDecl] -> IO ()
writeCFile nf code = do
    let cFile = L.unpack nf ++ ".cpp"
    writeFile cFile $ microBitIncludes ++ renderC code

compile :: L.Text -> L.Text -> IO ()
compile source newFile = do
    let res = parseProgram "from source" source
    case res of
      Right prog -> writeCFile newFile $ map compileMicroML prog
      Left err  -> error $ show err
