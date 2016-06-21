module Parser (
    parseExpr
  , parseProgram
) where

import Syntax
import Lexer
import Data.Functor.Identity (Identity)
import Data.Char (isLower, isUpper)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Control.Monad (void)

type MLParser = ParsecT Dec String Identity
type FileName = String
type Input = String

varName :: MLParser String
varName = do name@(n:_) <- identifier
             if isLower n 
                then return name
                else fail "a variable must start with a lowercase letter"

constructorName :: MLParser String
constructorName = do name@(n:_) <- identifier
                     if isUpper n 
                        then return name
                        else fail "a constructor must begin with a capital letter" 

variable :: MLParser Expr
variable = do
    x <- varName
    return $ Var x

constructor :: MLParser Expr
constructor = do
    x <- constructorName
    return $ Constructor x

number = do
    n <- integer
    return (Lit (Number n))

double = do
    n <- float
    return (Lit (Double n))

------------------------
-- EXPRESSION PARSERS --
------------------------

-- boolean operators
bExpr = makeExprParser bool boolOps

bool :: MLParser Expr
bool = parens bExpr
    <|> (reservedWord "true" *> pure (Lit (Boolean True)))
    <|> (reservedWord "false" *> pure (Lit (Boolean False)))

boolOps :: [[Operator MLParser Expr]]
boolOps =
    [ [ InfixL (reservedWord "and" *> pure (Op OpAnd))
      , InfixL (reservedWord "or"  *> pure (Op OpOr))] ]

-- arithemtic operators
aExpr = makeExprParser aTerm arithOps

aTerm :: MLParser Expr
aTerm = parens aExpr
    <|> try double
    <|> try number

arithOps :: [[Operator MLParser Expr]]
arithOps =
    [ [ InfixL (symbol "*" *> pure (Op OpMul))
      , InfixL (symbol "-" *> pure (Op OpSub))
      , InfixL (symbol "/" *> pure (Op OpDiv))
      , InfixL (symbol "%" *> pure (Op OpMod))
      , InfixL (symbol "+" *> pure (Op OpAdd))
      , InfixL (symbol "^" *> pure (Op OpExp))] ]

-- relational operators
rExpr = makeExprParser rTerm relationOps

rTerm :: MLParser Expr
rTerm = parens rExpr
    <|> try bool
    <|> aTerm

relationOps :: [[Operator MLParser Expr]]
relationOps = 
    [ [ InfixL (symbol "<=" *> pure (Op OpLe))
      , InfixL (symbol ">=" *> pure (Op OpGe))
      , InfixL (symbol "<"  *> pure (Op OpLt))
      , InfixL (symbol ">"  *> pure (Op OpGt))
      , InfixL (symbol "==" *> pure (Op OpEq))]]

---------------------
-- GENERAL PARSERS --
---------------------

atomicExpr :: MLParser Expr
atomicExpr = parens expr
         <|> bExpr
         <|> aExpr
         <|> rExpr
         <|> ifThenElse
         <|> lambda
         <|> variable

lambda :: MLParser Expr
lambda = do
  void $ symbol "\\"
  args <- many identifier
  void arrow
  body <- expr
  return $ foldr Lam body args

ifThenElse :: MLParser Expr
ifThenElse = do
    reservedWord "if"
    cond <- atomicExpr
    reservedWord "then"
    tr <- atomicExpr
    reservedWord "else"
    fl <- atomicExpr
    return $ If cond tr fl

expr :: MLParser Expr
expr = do
    es <- some atomicExpr
    return $ foldl1 App es

type Binding = (String, Expr)

letDecl :: MLParser Binding
letDecl = do
    reservedWord "let"
    name <- varName
    args <- many varName
    void $ symbol "="
    body <- expr
    return (name, foldr Lam body args)

val :: MLParser Binding
val = do
    ex <- expr
    return ("it", ex)

decl :: MLParser Binding
decl = try letDecl <|> val

topLevel :: MLParser Binding
topLevel = do 
    x <- decl
    void $ optional $ symbol ";"
    return x

program :: MLParser [Binding]
program = many topLevel

parseExpr :: Input -> Either (ParseError Char Dec) Expr
parseExpr = parse (contents expr) "<stdin>" 

parseProgram :: FileName -> Input -> Either (ParseError Char Dec) [Binding]
parseProgram = parse (contents program) 

