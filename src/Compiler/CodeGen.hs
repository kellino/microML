{-# LANGUAGE OverloadedStrings #-}

module Compiler.CodeGen where

import MicroML.Syntax
import MicroML.Parser
import Compiler.MicroBitHeader
import Compiler.CallGraph

import qualified Data.Text.Lazy as L
import qualified Data.Map as Map
import Data.Char (toLower)

import Text.PrettyPrint
import Text.Parsec (ParseError)

import System.FilePath

import Control.Monad.Identity
import Control.Monad.Except

type Compiler a = ExceptT String Identity a

type UserCode = Map.Map String Expr

genTopLevel :: UserCode -> (String, Expr) -> Compiler Doc
genTopLevel env ("main", expr) = generateMain env expr
genTopLevel env (nm, expr) = generateFunc env nm expr

generateMain :: UserCode -> Expr -> Compiler Doc
generateMain env ex = do
    ex' <- genBody env ex
    return $ "init main()" <> bracesNewLine (bitInit <> ex' <> fiber)

generateFunc :: UserCode -> Name -> Expr -> Compiler Doc
generateFunc env nm ex =  do
    ex' <- genBody env ex
    return $ text nm <> "()" <> bracesNewLine ex'

genBody :: UserCode -> Expr -> Compiler Doc
genBody env ex = 
    case ex of
         (Lit (LInt n))     -> return $ integer n
         (Lit (LDouble d))  -> return $ double d
         (Lit (LChar c))    -> return $ char c
         (Lit (LString x))  -> return $ doubleQuotes $ text x
         (Lit (LBoolean x)) -> return $ text . map toLower . show $ x
         (Var x)            -> do
             let found = Map.lookup x microBitAPI
             case found of
                  Nothing  -> case Map.lookup x env of
                                   Nothing -> error "not found"
                                   Just r -> genBody env r
                  Just r   -> return r  
         (Let nm e1 e2)     -> do 
                  e1' <- generateCExpr env nm e1
                  e2' <- genBody env e2
                  return $ e1' <> e2'
         (App x xs)   -> do
             x' <- genBody env x
             xs' <- genBody env xs
             return $ x' <> xs'
         x                 -> throwError $ "cannot genBody the expression " ++ show x

semiWithNewLine :: Doc
semiWithNewLine = semi <> "\n"

parensWithSemi :: Doc -> Doc
parensWithSemi d = parens d <> semi <> "\n"

bracesNewLine :: Doc -> Doc
bracesNewLine d = braces ("\n\t" <> d) <> "\n"

generateCExpr :: UserCode -> Name -> Expr -> Compiler Doc
generateCExpr env nm e1 = do
    ty <- getType e1
    let nm' = text nm
    e1' <- genBody env e1
    return $ ty <> nm' <> " = " <> e1' <> semiWithNewLine

getType :: Expr -> Compiler Doc
getType (Lit (LInt _)) = return $ "int" <> space
getType (Lit (LDouble _)) = return $ "double" <> space
getType (Lit (LString _)) = return $ "ManagedString" <> space
getType (Lit (LChar _)) = return $ "char" <> space
getType (Lit (LBoolean _)) = return $ "bool" <> space
getType _ = throwError "cannot get type of this expression"

runCompiler :: ExceptT e Identity a -> Either e a
runCompiler ev = runIdentity (runExceptT ev)

codegen :: [(String, Expr)] -> Compiler [Doc]
codegen prog = 
    let prog' = checkForDuplicates prog
        code  = Map.fromList prog'
        in mapM (genTopLevel code) prog'

hoistError :: Either ParseError [(String, Expr)] -> [(String, Expr)]
hoistError (Right val) = val
hoistError (Left err) = error $ "\ESC[31;1mParse Error\ESC[0m: " ++ show err

writeToFile :: L.Text -> [Doc] -> IO ()
writeToFile dest code = do
    let safe = fst $ splitExtension $ L.unpack dest
    let cFile = safe ++ ".cpp"
    let code' = foldr (<>) "" code
    writeFile cFile $ render (microBitIncludes <> code')

compile :: L.Text -> L.Text -> String -> IO ()
compile source dest filename = do
    let res = hoistError $ parseProgram filename source
    case runCompiler $ codegen res of
         Left e -> print e
         Right r -> writeToFile dest r
