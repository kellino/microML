{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- just a sketch at the moment, and borrowing a lot of code from
-- write you a scheme v2 by Stephen Diehl and Julien Blanchard

module Eval where

import Parser 
import Syntax
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import Text.Megaparsec.Error (parseErrorPretty)

data MLError = MLError String | MathsError String deriving Show

newtype Eval a = Eval { unEval :: ReaderT SymTable (ExceptT MLError IO) a }
    deriving (Monad, Functor, Applicative, MonadReader SymTable, MonadError MLError, MonadIO)

type SymTable = Map.Map String Expr

runAppT :: MonadIO m => SymTable -> Eval b -> ExceptT MLError m b
runAppT code action = do
    res <- liftIO $ runExceptT $ runReaderT (unEval action) code
    ExceptT $ return $ case res of
                         Left b  -> Left b
                         Right a -> Right a

testEnv = Map.fromList [("x", Number 42)]

process :: String -> IO ()
process input = do 
    out <- runExceptT $ runAppT testEnv $ textToEval input
    either print print out

textToEval :: String -> Eval Expr
textToEval input = either throwError eval (runParse_ input)

runParse_ :: String -> Either MLError Expr
runParse_ input = case readExpr input of
                    Left err -> Left $ MLError $ parseErrorPretty err
                    Right val -> Right val

----------------
-- EVALUATION --
----------------

eval :: Expr -> Eval Expr
eval (Con name) = return $ Con name
eval (Number i) = return $ Number i
eval (Double i) = return $ Double i
eval (StringLit str) = return $ StringLit str
eval (Boolean b) = return $ Boolean b
eval (Char c) = return $ Char c
--eval (Def (Var x) exp) = return $ setVar x exp
-- arithmetic
-- incredibly ugly code here, but don't yet know how to make this more generic
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
