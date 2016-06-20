{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- just a sketch at the moment, and borrowing a lot of code from
-- write you a scheme v2 by Stephen Diehl and Julien Blanchard

module Eval where

import Parser 
import Syntax
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map.Strict as Map
import Text.Megaparsec.Error (parseErrorPretty)

newtype Eval a = Eval { unEvaled :: ReaderT SymTable (ExceptT MLError IO) a }
    deriving (Monad, Functor, Applicative, MonadReader SymTable, MonadError MLError, MonadIO)

type SymTable = Map.Map String Expr

runAppT :: MonadIO m => SymTable -> Eval b -> ExceptT MLError m b
runAppT code action = do
    res <- liftIO $ runExceptT $ runReaderT (unEvaled action) code
    ExceptT $ return $ case res of
                         Left b  -> Left b
                         Right a -> Right a

-- how do we update this on the fly? 
table :: SymTable
table = Map.empty

process :: String -> IO ()
process input = do 
    out <- runExceptT $ runAppT table $ textToEval input
    either print print out

textToEval :: String -> Eval Expr
textToEval input = either throwError eval (runParse_ input)

runParse_ :: String -> Either MLError Expr
runParse_ input = case readExpr input of
                    Left err -> Left $ MLError $ parseErrorPretty err
                    Right val -> Right val

getVar :: Expr -> Eval Expr
getVar (Var x) = do
    env <- ask
    case Map.lookup x env of
      Just y -> return y
      Nothing -> throwError $ NotSet "this variable has not yet been set"

setVar :: Expr -> Eval Expr
setVar (Def (Var x) exp) = do
    env <- ask
    case Map.lookup x env of 
      Just _ -> throwError $ AlreadySet "this variable has already been assigned a value"
      Nothing -> local (const $ Map.insert x exp env) (eval exp)
      -- Nothing -> Map.insert x exp env >> (eval exp)

----------------
-- EVALUATION --
----------------

eval :: Expr -> Eval Expr
-- primitives
eval exp@(Var _) = getVar exp
eval def@(Def _ _) = setVar def
eval (Con name) = return $ Con name
eval (Number i) = return $ Number i
eval (Double i) = return $ Double i
eval (StringLit str) = return $ StringLit str
eval (Boolean b) = return $ Boolean b
eval (Char c) = return $ Char c
eval (Tuple xs) = return $ Tuple xs
eval (TypeSig _ _) = throwError $ Unsupported "Unfortunately, you can't enter a type signature at the prompt"
-- boolean operators
eval (Not x) = return $ Not x
eval (PrimBinOp OpOr (Boolean a) (Boolean b)) = return $ Boolean $ a || b
eval (PrimBinOp OpAnd (Boolean a) (Boolean b)) = return $ Boolean $ a && b
eval (PrimBinOp OpEq a b) = do
    a' <- eval a
    b' <- eval b
    return $ Boolean $ a' == b' 
eval (PrimBinOp OpLt a b) = do
    a' <- eval a
    b' <- eval b
    return $ Boolean $ a' < b'

-- arithmetic: incredibly ugly code here, but don't yet know how to make this more generic
eval (PrimBinOp OpAdd unev1 unev2) = do
    evaled1 <- eval unev1
    evaled2 <- eval unev2
    return $ evaled1 `add` evaled2
eval (PrimBinOp OpSub unev1 unev2) = do
    evaled1  <- eval unev1
    evaled2 <- eval unev2
    return $ evaled1 `sub` evaled2
eval (PrimBinOp OpMul unev1 unev2) = do
    evaled1  <- eval unev1
    evaled2 <- eval unev2
    return $ evaled1 `mul` evaled2
eval (PrimBinOp OpDiv unev1 unev2) = do
    evaled1  <- eval unev1
    evaled2 <- eval unev2
    evaled1 `div'` evaled2
eval (PrimBinOp OpMod unev1 unev2) = do
    evaled1  <- eval unev1
    evaled2 <- eval unev2
    evaled1 `mod'` evaled2
eval (PrimBinOp OpExp unev1 unev2) = do
    evaled1 <- eval unev1
    evaled2 <- eval unev2
    evaled1 `exp'` evaled2

eval (IfThenElse cond exp1 exp2) = do
    c <- eval cond
    e1 <- eval exp1
    e2 <- eval exp2
    return $ IfThenElse c e1 e2

eval _ = throwError $ MLError "not yet supported"

add :: Expr -> Expr -> Expr
add (Number a) (Number b) = Number $ a + b
add (Double a) (Double b) = Double $ a + b
add (Number a) (Double b) = Double $ realToFrac a + b
add (Double a) (Number b) = Double $ a + realToFrac b

sub :: Expr -> Expr -> Expr
sub (Number a) (Number b) = Number $ a - b
sub (Double a) (Double b) = Double $ a - b
sub (Number a) (Double b) = Double $ realToFrac a - b
sub (Double a) (Number b) = Double $ a - realToFrac b

mul :: Expr -> Expr -> Expr
mul (Number a) (Number b) = Number $ a * b
mul (Double a) (Double b) = Double $ a * b
mul (Number a) (Double b) = Double $ realToFrac a * b
mul (Double a) (Number b) = Double $ a * realToFrac b

-- simplistic handling of division
div' :: Expr -> Expr -> Eval Expr
div' (Number a) (Number b) = return $ Double $ realToFrac a / realToFrac b
div' (Double a) (Double b) = return $ Double $ a / b
div' (Number a) (Double b) = return $ Double $ realToFrac a / b
div' (Double a) (Number b) = return $ Double $ a / realToFrac b
div' _ _ = throwError $ MathsError "are you sure you've only entered numbers?"

mod' :: Expr -> Expr -> Eval Expr
mod' (Number a) (Number b) = return $ Number $ a `mod` b
mod' _ _ = throwError $ MathsError "the modulo operator only works on whole numbers"

exp' :: Expr -> Expr -> Eval Expr
exp' (Number a) (Number b) = return $ Number $ a^b
exp' (Number a) (Double b) = return $ Double $ realToFrac a**b
exp' (Double a) (Number b) = return $ Double $ a ^ b
exp' (Double a) (Double b) = return $ Double $ a**b
