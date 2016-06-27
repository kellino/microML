module Language.Parser where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Expr as Ex
import Data.Char (isLower, isUpper)
import Data.List (sortBy, groupBy)
import Data.Function (on)
import Control.Monad.Identity (Identity)

import Language.Syntax
import Language.Lexer


varName :: ParsecT String () Identity String
varName = do
    name@(n:_) <- identifier
    if isLower n
       then return name
       else fail "a variable name must start with a lowercase letter"

conName :: ParsecT String () Identity String
conName = do
    name@(n:_) <- identifier
    if isUpper n
       then return name
       else fail "a constructor name must start with a capital letter"

expr :: ParsecT String () Identity Expr
expr = do
    exs <- many1 term
    return $ foldl1 App exs

lambda :: ParsecT String () Identity Expr
lambda = do
    reservedOp "\\"
    args <- many identifier
    reservedOp "->"
    body <- expr
    return $ foldr Lam body args

variable :: ParsecT String () Identity Expr
variable = do
    x <- varName
    return $ Var x

constructorName :: ParsecT String () Identity Expr
constructorName = do
    x <- conName
    return $ Constructor x

letin :: ParsecT String () Identity Expr
letin = do
    reserved "let"
    x <- varName
    reservedOp "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return $ Let x e1 e2
    
addOperator a = modifyState $ \(ParseState ops) -> ParseState (a : ops)

makeOperatorTable :: ParseState -> [[Ex.Operator String () Identity Expr]]
makeOperatorTable (ParseState ops) = 
    map (map toParser) $ 
        groupBy ((==) `on` oprec) $
            sortBy (flip compare `on` oprec) ops

toParser (OperatorDef ass _ tok) = case ass of
    OpLeft  -> infixOp tok (Binop tok) (toAssoc ass)
    OpRight -> infixOp tok (Binop tok) (toAssoc ass)
    where 
        toAssoc OpLeft = Ex.AssocLeft
        toAssoc OpRight = Ex.AssocRight

infixOp x f = Ex.Infix (reservedOp x >> return f)

term = do
    tbl <- getState
    let table = makeOperatorTable tbl
    Ex.buildExpressionParser table aexp

aexp = letin
    <|> lambda
    <|> variable
    <|> parens expr

letdecl = do
    e <- expr
    return $ LetDecl e

opLeft = do
    reserved "infixL"
    prec <- integer
    symbol <- parens operator
    let op = OperatorDef OpLeft prec symbol
    addOperator op
    return $ OpDecl op

decl =
    try letdecl
    <|> opLeft

topLevel = do
    x <- decl
    optional semi
    return x

modl = many topLevel

parseModule = runParser (contents modl) (ParseState []) 
