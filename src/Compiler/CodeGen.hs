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
import Control.Monad.Reader
import Control.Monad.Except

type Compiler a = ReaderT UserCode (ExceptT String Identity) a

type UserCode = Map.Map String Expr

genTopLevel ::  (String, Expr) -> Compiler Doc
genTopLevel ("main", expr) = generateMain expr
genTopLevel (nm, expr) = generateFunc nm expr

generateMain ::  Expr -> Compiler Doc
generateMain ex = do
    ex' <- genBody ex
    return $ "init main()" <> bracesNewLine (bitInit <> ex' <> fiber)

generateFunc :: Name -> Expr -> Compiler Doc
generateFunc nm ex =  do
    ex' <- genBody ex
    return $ text nm <> "()" <> bracesNewLine ex'

genBody :: Expr -> Compiler Doc
genBody ex = 
    case ex of
         (Lit (LInt n))     -> return $ integer n
         (Lit (LDouble d))  -> return $ double d
         (Lit (LChar c))    -> return $ char c
         (Lit (LString x))  -> return $ doubleQuotes $ text x
         (Lit (LBoolean x)) -> return $ text . map toLower . show $ x
         Var x              -> 
             case Map.lookup x microBitAPI of
                  Nothing -> do
                      env <- ask
                      case Map.lookup x env of
                           Nothing -> throwError $ "Error: binding " ++ x ++ " has not been found"
                           Just r  -> genBody r
                  Just r  -> return r
         (Let nm e1 e2)     -> do 
             env <- ask
             let e1' = e1
             local (const (Map.insert nm e1' env)) (genBody e2)
         (App x xs)   -> do
             x' <- genBody x
             xs' <- genBody xs
             return $ x' <> parensWithSemi xs'
         x                 -> throwError $ "cannot genBody the expression " ++ show x

semiWithNewLine :: Doc
semiWithNewLine = semi <> "\n"

parensWithSemi :: Doc -> Doc
parensWithSemi d = parens d <> semi <> "\n"

bracesNewLine :: Doc -> Doc
bracesNewLine d = braces ("\n\t" <> d) <> "\n"

getType :: Expr -> Compiler Doc
getType (Lit (LInt _)) = return $ "int" <> space
getType (Lit (LDouble _)) = return $ "double" <> space
getType (Lit (LString _)) = return $ "ManagedString" <> space
getType (Lit (LChar _)) = return $ "char" <> space
getType (Lit (LBoolean _)) = return $ "bool" <> space
getType _ = throwError "cannot get type of this expression"

runCompiler :: UserCode -> Compiler a -> Either String a
runCompiler env ev = runIdentity (runExceptT (runReaderT ev env))

codegen :: [(String, Expr)] -> Compiler [Doc]
codegen = mapM genTopLevel

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
    let code = checkForDuplicates res
    case runCompiler (Map.fromList code) $ codegen code of
         Left e -> print e
         Right r -> writeToFile dest r
