{-# LANGUAGE OverloadedStrings #-}

module Compiler.CodeGen where

import Compiler.MicroBitHeader
import Compiler.CHelper
import MicroML.Syntax
import MicroML.Parser 
import MicroML.Typing.Env
import MicroML.Typing.Infer
import Repl.Pretty()

import Language.C.DSL 
import Control.Monad.State.Strict
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Except
import Control.Applicative

import qualified Data.Text.Lazy as L
import Data.Either (rights)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.String (fromString)

import System.Exit

--------------------
-- COMPILER TYPES --
--------------------

type FuncName = String
type CErrorMsg = String
type FileName = L.Text
type CompEnv = Map.Map String Expr
type Compiler a = ReaderT CompEnv (ExceptT CErrorMsg Identity) a
-- typing environment
data TypeState = TypeState { typeState :: Env }

type CState a = (StateT TypeState IO) a

--------------
-- COMPILER --
--------------

runCompiler :: CompEnv -> Compiler a -> Either CErrorMsg a
runCompiler env ev = runIdentity (runExceptT (runReaderT ev env))

hoistError :: Show a => Either a t -> t
hoistError (Right vl) = vl
hoistError (Left err) = error $ show err

generate :: (String, Expr) -> Compiler CExtDecl
generate (nm, expr) = 
    case expr of
      -- if there's only a name and a Lit expression, then it must be a variable declaration
      Lit _            -> return $ generateLit nm expr
      --If cond tr fls   -> return $ ternary (gen cond) (gen tr) (gen fls)
      Nil              -> undefined
      App (Var n) body -> return $ export $ do
          let (func, retty) = fromJust $ Map.lookup n microBitAPI
          if nm == "main" 
             then makeMain func (resolve body)
             else fun [getRetType retty] nm [charPtr "str"] $ hBlock [ func # ["str"] ]
      -- function definition
      Lam n e -> return $ generateLam n e
      _ -> throwError "something strange has happened"

gen :: Expr -> CExpr
gen = undefined

generateLam :: String -> Expr -> CExtDecl
generateLam _ _ = undefined

-- generates simple variable declarations of the form
-- int x = 2;
generateLit :: String -> Expr -> CExtDecl
generateLit nm e1 = 
    let newNm = fromString nm
     in case e1 of  
          (Lit (LInt n))     -> export $ int newNm .= fromInteger n
          (Lit (LDouble d))  -> export $ double newNm .= realToFrac d
          (Lit (LChar c))    -> export $ char newNm .= chr c
          (Lit (LString st)) -> export $ charPtr newNm .= str st

getRetType :: RetTy -> CDeclSpec
getRetType rt = fromJust $ Prelude.lookup rt table
    where table = 
            [ ("void"   , voidTy)
            , ("int"    , intTy)
            , ("double" , doubleTy)
            , ("char"   , charTy) ]

makeFunProtos :: CExtDecl -> Maybe CExtDecl
makeFunProtos (CFDefExt (CFunDef specs declr _ _ a)) =
    Just . export $ CDecl specs [(Just declr, Nothing, Nothing)] a
makeFunProtos _ = Nothing

makeMain :: CExpr -> [CExpr] -> CFunDef
makeMain func bdy = 
    fun [intTy] "main" [] $ hBlock [
        microBitInit
      , func # bdy                            
      , releaseFiber
    ]

resolve :: Expr -> [CExpr]
resolve e = 
    case e of 
      Lit (LInt n)     -> [int' n]
      Lit (LDouble d)  -> [db d]
      Lit (LChar c)    -> [chr c]
      Lit (LString st) -> [str st]

checkTypes :: [(String, Expr)] -> CState ()
checkTypes prog = do
    st <- get
    let typeState' = hoistError $ inferTop (typeState st) prog
    let st' = st { typeState = typeState' `mappend` typeState st }
    put st'

---------------------
-- PRETTY PRINTING --
---------------------

renderC :: [CExtDecl] -> String
renderC = newlines . concatMap (show . pretty)

newlines :: String -> String
newlines xs = [c | v <- xs, c <- if v == ';' || v == '}' then v:"\n" else [v]]

writeCFile :: L.Text -> [CExtDecl] -> IO ()
writeCFile nf code = do
    let cFile = L.unpack nf ++ ".cpp"
    writeFile cFile $ microBitIncludes ++ renderC code

----------------------------
-- MAIN COMPILER FUNCTION --
----------------------------

compile :: L.Text -> L.Text -> IO ()
compile source fn = do
    let res = hoistError $ parseProgram "from source" source
    mapM_ print res
    if null res
       then die $ red ++ "Exit Failure: " ++ unred ++ "the given file was empty, so there's nothing to compile!"
       else do 
           let code = map (runCompiler Map.empty . generate) res
           writeCFile fn $ rights code
