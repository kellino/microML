module Parser (
    parseProgram
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

number :: MLParser Expr
number = do
    n <- integer
    return (Lit (Number n))

double :: MLParser Expr
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
    <|> variable
    <|> try double
    <|> number

arithOps :: [[Operator MLParser Expr]]
arithOps =
     [ [ Prefix (symbol "-" *> pure UnaryMinus)
       , Prefix (symbol "+" >> return id) ]
     , [ InfixL (symbol "^" *> pure (Op OpExp)) ] 
     , [ InfixL (symbol "*" *> pure (Op OpMul))
     ,   InfixL (symbol "/" *> pure (Op OpDiv))
     ,   InfixL (symbol "%" *> pure (Op OpMod)) ]
     , [ InfixL (symbol "+" *> pure (Op OpAdd))
     ,   InfixL (symbol "-" *> pure (Op OpSub)) ] ]

-- relational operators
rExpr = makeExprParser rTerm relationOps

rTerm :: MLParser Expr
rTerm = parens rExpr
    <|> try bool
    <|> try aExpr

relationOps :: [[Operator MLParser Expr]]
relationOps = 
    [ [ InfixL (symbol "<=" *> pure (Op OpLe))
      , InfixL (symbol ">=" *> pure (Op OpGe))
      , InfixL (symbol "<"  *> pure (Op OpLt))
      , InfixL (symbol ">"  *> pure (Op OpGt))
      , InfixL (symbol "==" *> pure (Op OpEq)) ] ]

---------------------
-- GENERAL PARSERS --
---------------------

atomicExpr :: MLParser Expr
atomicExpr = 
             parens expr
         <|> try rExpr
         <|> try aExpr
         <|> bExpr
         <|> ifThenElse
         <|> lambda
         <|> variable
         <|> stringLit
         <|> charLit
         <|> list

charLit :: MLParser Expr
charLit = do
    void $ symbol "'"
    c <- alphaNumChar <|> spaceChar
    void $ symbol "'"
    return $ Lit $ Char c

stringLit :: MLParser Expr
stringLit = do
    void $ symbol "\""
    str <- many $ alphaNumChar <|> spaceChar
    void $ symbol "\""
    return $ Lit $ String str

list :: MLParser Expr
list = do
    contents <- brackets $ atomicExpr `sepBy` comma
    return $ List contents

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

letrecDecl :: MLParser Binding
letrecDecl = do
    reservedWord "let"
    reservedWord "rec"
    name <- varName
    args <- many varName
    void $ symbol "="
    body <- expr
    return (name, FixPoint $ foldr Lam body (name:args))

mainDecl :: MLParser Binding
mainDecl = do
    reservedWord "main"
    void $ symbol "="
    body <- expr
    return ("main", body)

val :: MLParser Binding
val = do
    ex <- expr
    return ("it", ex) --  same syntax here as in ghci

decl :: MLParser Binding
decl = try letrecDecl <|> try letDecl <|> val <|> mainDecl

topLevel :: MLParser Binding
topLevel = do 
    x <- decl
    void $ optional $ symbol ";"
    return x

program :: MLParser [Binding]
program = many topLevel

parseProgram :: FileName -> Input -> Either (ParseError Char Dec) [Binding]
parseProgram = parse (contents program) 

