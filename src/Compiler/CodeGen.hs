{-# LANGUAGE OverloadedStrings #-}

module Compiler.CodeGen where

import qualified Data.Text.Lazy as L

import MicroML.Parser (parseProgram)
import MicroML.Syntax

import Compiler.MicroBitHeader
--import Compiler.Failure
import Compiler.CallGraph

import Text.PrettyPrint
import Text.Parsec (ParseError)
import qualified Data.Map as Map

parensWithSemi :: Doc -> Doc
parensWithSemi d = parens d <> semi <> "\n"

bracesNewLine :: Doc -> Doc
bracesNewLine d = braces ("\n\t" <> d) <> "\n"

writeToFile :: L.Text -> [Doc] -> IO ()
writeToFile dest code = do
    let cFile = L.unpack dest ++ ".cpp"
    let code' = foldr (<>) "" code
    writeFile cFile $ render (microBitIncludes <> code')

codegen :: [(String, Expr)] -> [Doc]
codegen = map genTopLevel . validDefs . checkForDuplicates

genTopLevel :: (String, Expr) -> Doc
genTopLevel ("main", expr) = generateMain expr

generateMain :: Expr -> Doc
generateMain ex = "int main()" <> bracesNewLine (bitInit <> genBody ex <> fiber)

genBody :: Expr -> Doc
genBody ex = 
    case ex of
         (Lit (LInt n))    -> integer n
         (Lit (LDouble d)) -> double d
         (Lit (LChar c))   -> char c
         (Lit (LString x)) -> doubleQuotes $ text x
         (Var _)           -> undefined
         (Let nm e1 e2)    -> generateCExpr nm e1 <> genBody e2
         (App (Var x) xs)  -> do
             let found = Map.lookup x microBitAPI
             case found of
                  Nothing  -> error "instruction not found"
                  Just r   -> r <> parensWithSemi (genBody xs)

generateCExpr :: Name -> Expr -> Doc
generateCExpr nm e1 = 
    getType e1 <> text nm <> " = " <> genBody e1

getType :: Expr -> Doc
getType (Lit (LInt _)) = "int"
getType (Lit (LDouble _)) = "double"
getType (Lit (LString _)) = "ManagedString"
getType (Lit (LChar _)) = "char"
getType (Lit (LBoolean _)) = "bool"


compileMicroML :: Either ParseError [(String, Expr)] -> [Doc]
compileMicroML res = 
    case res of
         Left err -> error $ show err
         Right r -> codegen r

compile :: L.Text -> L.Text -> String -> IO ()
compile source dest filename = do
    let res = parseProgram filename source
    print res -- debugging
    let compRes = compileMicroML res
    writeToFile dest compRes
