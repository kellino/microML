{-# LANGUAGE OverloadedStrings #-}

module Compiler.CodeGen where

import MicroML.Syntax
import MicroML.Parser
import Compiler.MicroBitHeader
import Compiler.CallGraph
import Compiler.Failure
import Compiler.PrettyCPP
import MicroML.Typing.Infer
import MicroML.Typing.Env
import qualified MicroML.Typing.Env as Env
import Repl.Pretty()

import qualified Data.Text.Lazy as L
import qualified Data.Map as Map
import Data.Char (toLower)

import Text.PrettyPrint hiding (equals)

import System.FilePath
import System.Directory

import Control.Monad.RWS hiding ((<>))
import Control.Monad.Except

---------------
-- DATATYPES --
---------------

type Compiler a = (RWST CodeState [Doc] CompilerState (Except Failure) a)

data CompilerState = CompilerState { count :: Int }

initCompiler :: CompilerState
initCompiler = CompilerState { count = 0 }

type UserCode = Map.Map String Expr

data CodeState = CodeState 
            { userCode :: UserCode
            , typeEnv :: Env }

initState :: [(String, Expr)] -> CodeState
initState code = CodeState (Map.fromList code) (genTypeEnv code)

genTypeEnv :: [(String, Expr)] -> Env
genTypeEnv cd = 
    case inferTop Env.empty cd of
         Left err -> error "type error"
         Right val -> val

-------------------------
-- CPP CODE GENERATION -- 
-------------------------

-- generate fresh names for unnamed bindings in microML (eg. when using the >> operator)
fresh :: Compiler Doc
fresh = do
    s <- get
    put s{ count = count s + 1 }
    return $ text (letters !! count s)
    where letters = [1..] >>= flip replicateM ['a' .. 'z']

genTopLevel ::  (String, Expr) -> Compiler Doc
genTopLevel ("main", expr) = generateMain expr
genTopLevel (nm, expr) = generateFunc nm expr

generateMain ::  Expr -> Compiler Doc
generateMain ex = do
    ex' <- genBody ex
    return $ "int main()" <> bracesNewLine (bitInit <> ex' <> fiber)

generateFunc :: Name -> Expr -> Compiler Doc
generateFunc nm ex =  do
    ex' <- genBody ex
    return $ funcType <> nm' <> "()" <> bracesNewLine ("return " <> ex' <> ";\n")
        where nm' = text nm

genIf :: Expr -> Compiler Doc
genIf (If cond tr fls) = do
    cond' <- genBody cond
    tr'   <- genBody tr
    fls'  <- genBody fls
    return $ "if" <> parens cond' <> bracesNewLine tr' <> "else " <> fls'

-- | placeholder !!! 
funcType :: Doc
funcType = "ManagedString " 

genBody :: Expr -> Compiler Doc
genBody ex = 
    case ex of
         (Lit (LInt n))     -> return $ integer n
         (Lit (LDouble d))  -> return $ double d
         (Lit (LChar c))    -> return $ quotes $ char c
         (Lit (LString x))  -> return $ doubleQuotes $ text x
         (Lit (LBoolean x)) -> return $ text . map toLower . show $ x
         Lam _ _            -> failGen "" "not written yet"
         Var x              -> 
             case Map.lookup x microBitAPI of
                  Nothing -> return $ text x
                  Just r  -> return r
         Let nm e1 e2       -> do
             ret <- getType e1
             e1' <- genBody e1
             e2' <- genBody e2
             return $ ret <> text nm <> " = " <> e1' <> semiWithNewLine <> e2'
         ifstat@If{}        -> genIf ifstat
         App x xs           -> do
             x' <- genBody x
             xs' <- genBody xs
             return $ x' <> parensWithSemi xs'
         BinOp op e1 e2    -> do
             e1' <- genBody e1
             e2' <- genBody e2
             case op of
                  OpPipe   -> pipeFuncs e1 e2
                  _        -> return $ e1' <> ppr op <> e2'
         _                 -> failGen (showText ex) ": this operation is presently unsupported"

getType :: Expr -> Compiler Doc
getType (Lit (LInt _)) = return $ "int" <> space
getType (Lit (LDouble _)) = return $ "double" <> space
getType (Lit (LString _)) = return $ "ManagedString" <> space
getType (Lit (LChar _)) = return $ "char" <> space
getType (Lit (LBoolean _)) = return $ "bool" <> space
getType x = failGen (showText x) ": unable to ascertain type of this expression"

pipeFuncs e1 e2 = do 
    b1 <- genBody e1 
    nm <- fresh
    b2 <- genBody (App e2 (Var $ render nm))
    return $ "ManagedString " <> nm <> " = " <> b1 <> "();\n" <> b2

hoistError :: Show a =>  Either a [(String, Expr)] -> [(String, Expr)]
hoistError (Right val) = val
hoistError (Left err) = error $ "\ESC[31;1mParse Error\ESC[0m: " ++ show err

validateExtension :: String -> String
validateExtension fl -- = if (snd . splitExtension) fl == ".cpp" then fl else (fst . splitExtension $ fl) ++ ".cpp"
    | extension == ".cpp" = fl
    | extension == "" = fl ++ ".cpp"
    | otherwise = error $ red ++ "File Extension Error: " ++ clear ++ extension ++ " is not a valid filetype for compiled microML.\n"
                        ++ "Please try either .cpp or don't add an extension"
    where extension = snd . splitExtension $ fl

writeToFile :: L.Text -> [Doc] -> IO ()
writeToFile dest code = do
    let cFile = validateExtension $ L.unpack dest
    let code' = foldr (<>) "" code
    writeFile cFile $ render (microBitIncludes <> code')
    -- if clang-format exists, then use it to clean up the formatting.
    formatPrintedFile cFile

codegen :: [(String, Expr)] -> Compiler [Doc]
codegen = mapM genTopLevel 
        . reachableFromMain 

runCompiler :: CodeState -> Compiler a -> Either Failure (a, [Doc])
runCompiler env m = runExcept $ evalRWST m env initCompiler

compile :: L.Text -> L.Text -> String -> IO ()
compile source dest filename = do
    let res = hoistError $ parseProgram filename source
    let code = checkForDuplicates res
    case runCompiler (initState code) $ codegen code of
         Left e -> print $ tellError e
         Right r -> writeToFile dest $ fst r
