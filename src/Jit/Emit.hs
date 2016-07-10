module Jit.Emit where

import qualified Data.Map as Map

import LLVM.General.Module
import LLVM.General.Context
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Control.Monad (forM_)
import Control.Monad.Except
import MicroML.Syntax
import Jit.Codegen

codegenTop :: Expr -> LLVM ()
codegenTop (Lam name body) = 
    define double name fnargs bls
    where
        fnargs = toSig body
        bls = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            forM_ body $ \a -> do
                var <- alloca double
                store var (local (AST.Name a))
                assign a var
            cgen body >>= ret
codegenTop exp =
    define double "main" [] blks
    where
        blks = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            cgen exp >>= ret

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

cgen :: Expr -> Codegen AST.Operand
cgen (Lit (LInt n))    = return $ cons $ C.Float (F.Double n)
cgen (Lit (LDouble d)) = return $ cons $ C.Float (F.Double d)
cgen (Var x)           = getVar x >>= load
cgen (Op op a b) = 
    case Map.lookup op binops of
      Just f -> do
          ca <- cgen a
          cb <- cgen b
          f ca cb
      Nothing -> error "No such operator"


binops = Map.fromList [
        ("+", fadd)                  
      , ("-", fsub)
      , ("*", fmul)
      , ("/", fdiv)
      , ("<", lt)
    ]

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
    test <- fcmp 
    uitofp double test

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
    liftError $ withModuleFromAST context newast $ \m -> do
        llstr <- moduleLLVMAssembly m
        putStrLn llstr
        return newast
    where
        modn = mapM codegenTop fns
        newast = runLLVM mod modn

