{-# LANGUAGE OverloadedStrings #-}

module Compiler.CodeGen where

import qualified Data.Text.Lazy as L

import MicroML.Parser (parseProgram)
import MicroML.Syntax

import Compiler.MicroBitHeader
--import Compiler.Failure
import Compiler.CallGraph

--import Control.Monad.State.Strict
import Text.PrettyPrint
import Text.Parsec (ParseError)
import qualified Data.Map as Map

--import Data.Maybe (fromMaybe)

type CodeEnv = Map.Map Name Expr

data CompilerState a = CompilerState { codeState :: CodeEnv }

instance Monoid (CompilerState a) where
    mempty = CompilerState Map.empty
    mappend (CompilerState a) (CompilerState b) = CompilerState $ Map.union a b

initState :: CompilerState CodeEnv
initState = CompilerState Map.empty

parensWithSemi :: Doc -> Doc
parensWithSemi d = parens d <> semi <> "\n"

semiWithNewLine :: Doc
semiWithNewLine = semi <> "\n"

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
                  Nothing  -> error "not found"
                  Just r   -> r <> parensWithSemi (genBody xs)

generateCExpr :: Name -> Expr -> Doc
generateCExpr nm e1 = 
    getType e1 <> text nm <> " = " <> genBody e1 <> semiWithNewLine

getType :: Expr -> Doc
getType (Lit (LInt _)) = "int" <> space
getType (Lit (LDouble _)) = "double" <> space
getType (Lit (LString _)) = "ManagedString" <> space
getType (Lit (LChar _)) = "char" <> space
getType (Lit (LBoolean _)) = "bool" <> space

compileMicroML :: Either ParseError [(String, Expr)] -> [Doc]
compileMicroML res = 
    case res of
         Left err -> error $ show err
         Right r -> codegen r

hoistError :: Either ParseError [(String, Expr)] -> [(String, Expr)]
hoistError (Right val) = val
hoistError (Left err) = error $ show err

compile :: L.Text -> L.Text -> String -> IO ()
compile source dest filename = do
    let res = parseProgram filename source
    let compRes = compileMicroML res
    writeToFile dest compRes
