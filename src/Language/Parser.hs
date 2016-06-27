{-# LANGUAGE OverloadedStrings #-}

module Language.Parser (
 -- parseExpr,
  parseModule
) where

import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Char (isLower, isUpper)
import qualified Data.Text.Lazy as L

import Language.Lexer
import Language.Syntax

varName = do
    name@(n:_) <- identifier
    if isLower n
       then return name
       else fail "a variable name must start with a lowercase letter"

constructorName = do
    name@(n:_) <- identifier
    if isUpper n
       then return name
       else fail "a \ESC[1mconstructor\ESC[0m must start with a capital letter"

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

variable :: Parser Expr
variable = varName >>= \n -> return $ Var n

constructor :: Parser Expr
constructor = constructorName >>= \c -> return $ Constructor c

number :: Parser Expr
number = integer >>= \n -> return (Lit (LInt n))

double :: Parser Expr
double = float >>= \d -> return (Lit (LDouble d))

bool :: Parser Expr
bool = (reserved "True" >> return (Lit (LBool True)))
    <|> (reserved "False" >> return (Lit (LBool False)))

{-fix :: Parser Expr-}
{-fix = do-}
  {-reservedOp "fix"-}
  {-x <- aexp-}
  {-return (Fix x)-}

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many varName
  reservedOp "->"
  body <- expr
  return $ foldr Lam body args

letin :: Parser Expr
letin = do
  reserved "let"
  x <- varName
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (Let x e1 e2)

letrecin :: Parser Expr
letrecin = do
  reserved "let"
  reserved "rec"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (Let x e1 e2)

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- aexp
  reservedOp "then"
  tr <- aexp
  reserved "else"
  fl <- aexp
  return (If cond tr fl)

aexp :: Parser Expr
aexp =
      parens expr
  <|> bool
  <|> number
  <|> ifthen
  <|> try letrecin
  <|> letin
  <|> lambda
  <|> variable

term :: Parser Expr
term = Ex.buildExpressionParser table aexp

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

table :: Operators Expr
table = [
    [
      infixOp "*" (Op Mul) Ex.AssocLeft
    ],
    [
      infixOp "+" (Op Add) Ex.AssocLeft
    , infixOp "-" (Op Sub) Ex.AssocLeft
    ],
    [
      infixOp "==" (Op Eql) Ex.AssocLeft
    ]
  ]

expr :: Parser Expr
expr = do
  es <- many1 term
  return (foldl1 App es)

type Binding = (String, Expr)

letdecl :: Parser Binding
letdecl = do
  reserved "let"
  name <- varName
  args <- many varName
  reservedOp "="
  body <- expr
  return (name, foldr Lam body args)

letrecdecl :: Parser (String, Expr)
letrecdecl = do
  reserved "let"
  reserved "rec"
  name <- varName
  args <- many identifier
  reservedOp "="
  body <- expr
  return (name, Fix $ foldr Lam body (name:args))

val :: Parser Binding
val = do
  ex <- expr
  return ("it", ex)

decl :: Parser Binding
decl = try letrecdecl <|> letdecl <|> val

top :: Parser Binding
top = do
  x <- decl
  optional semi
  return x

modl ::  Parser [Binding]
modl = many top

{-parseExpr :: L.Text -> Either ParseError Expr-}
{-parseExpr = parse (contents expr) "<stdin>"-}

parseModule ::  FilePath -> L.Text -> Either ParseError [(String, Expr)]
parseModule = parse (contents modl)
