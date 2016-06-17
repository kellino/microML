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

data MLError = Default String deriving Show

newtype Eval a = Eval { unEval :: ReaderT SymTable (ExceptT MLError IO) a }
    deriving (Monad, Functor, Applicative, MonadReader SymTable, MonadError MLError, MonadIO)

type SymTable = Map.Map String Expr

runAppT :: MonadIO m => SymTable -> Eval b -> ExceptT MLError m b
runAppT code action = do
    res <- liftIO $ runExceptT $ runReaderT (unEval action) code
    ExceptT $ return $ case res of
                         Left b  -> Left b
                         Right a -> Right a

testEnv = Map.fromList [("x", Num 42)]

process :: String -> IO ()
process input = do 
    out <- runExceptT $ runAppT testEnv $ textToEval input
    either print print out

textToEval :: String -> Eval Expr
textToEval input = either throwError eval (runParse_ input)

runParse_ :: String -> Either MLError Expr
runParse_ input = case readExpr input of
                    Left err -> Left $ Default $ show err
                    Right val -> Right val

eval :: Expr -> Eval Expr
eval (Con name) = return $ Con name
eval (Num i) = return $ Num i
eval (Double i) = return $ Double i
eval (StringLit str) = return $ StringLit str
eval (Boolean b) = return $ Boolean b
eval (Char c) = return $ Char c
-- simple arithmetic test
eval (PrimBinOp OpAdd (Num a) (Num b)) = return $ Num (a + b)
eval (PrimBinOp OpAdd (Double a) (Double b)) = return $ Double (a + b)
