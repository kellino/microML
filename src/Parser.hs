{-# LANGUAGE TupleSections #-}

module Parser (
        exprParser
      , readExpr
      , parseFromFile
        ) where

import Syntax
import Control.Monad (void)
import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String
import Text.Megaparsec.Expr

import Data.Functor.Identity (Identity)
import Data.Char (isLower, isUpper)

-------------------------
-- PROMPT HIGHLIGHTING --
-------------------------

promptBold :: String
promptBold = "\ESC[1m"

promptReset :: String
promptReset = "\ESC[0m"

promptRed :: String
promptRed = "\ESC[31m"

promptYellow :: String
promptYellow = "\ESC[33m"

---------------------------
-- SYMBOLS & ATOM VALUES --
---------------------------

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

-- using block comments in the style of sml
blockComment :: Parser ()
blockComment = L.skipBlockComment "(*" "*)"

-- space consumer for newlines
scn :: Parser ()
scn = L.space (void spaceChar) lineComment blockComment

-- space consumer for folds and blocks, i.e ignores newline characters
sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer = lexeme L.integer
float = lexeme L.float

-- symbol list
symbol = L.symbol sc
charLiteral = lexeme L.charLiteral
parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")
semicolon = symbol ";"
comma = symbol ","
dot = symbol "."
isType = symbol "::"

reservedWord :: String -> ParsecT Dec String Identity ()
reservedWord w = string w *> notFollowedBy alphaNumChar *> sc

-- a list of reserved words, not available for use as identifiers
reserved :: [String]
reserved = ["if", "then", "else", "true", "false", "let", "in", "and", "or", "not", "otherwise"]

identifier :: Parser String
identifier = lexeme (p >>= check)
    where 
        p = (:) <$> letterChar <*> many alphaNumChar
        check x = if x `elem` reserved
                     then fail $ promptRed ++ show x ++ promptReset ++ " is a \ESC[1mreserved word\ESC[0m and cannot be used as an \ESC[1midentifier\ESC[0m" 
                     else return x

stringLit :: ParsecT Dec String Identity Expr
stringLit = stringLit >>= \str -> return $ StringLit str
    where
        stringLit = char '"' >> manyTill charLiteral (char '"')

constructorName :: ParsecT Dec String Identity Expr
constructorName = do name@(n:_) <- identifier
                     if isUpper n 
                        then return (Con name)
                        else fail $ promptBold ++ "a constructor must begin with a capital letter" ++ promptReset 

varName :: ParsecT Dec String Identity String
varName = do name@(n:_) <- identifier
             if isLower n
                then return name
                else fail $ promptBold ++ "a variable must start with a lowercase letter" ++ promptReset

------------------------
-- Expression Parsers -- 
------------------------

aExpr, bExpr :: ParsecT Dec String Identity Expr
-- expression parser for arithmetic
aExpr = makeExprParser aTerm arithOps
-- expression parser for boolean operations
bExpr = makeExprParser bTerm boolOps
-- expression parser for list operations
lExpr = makeExprParser lTerm lOps

arithOps :: [[Operator (ParsecT Dec String Identity) Expr]]
arithOps =
    [ [ Prefix (symbol "-" >> return Neg) ]
     ,[ InfixL (symbol "*" *> pure (PrimBinOp OpMul))
      , InfixL (symbol "-" *> pure (PrimBinOp OpSub))
      , InfixL (symbol "/" *> pure (PrimBinOp OpDiv))
      , InfixL (symbol "%" *> pure (PrimBinOp OpMod))
      , InfixL (symbol "+" *> pure (PrimBinOp OpAdd))] ]

boolOps :: [[Operator (ParsecT Dec String Identity) Expr]]
boolOps =
    [ [ Prefix (reservedWord "not" *> pure Not) ]
     ,[ InfixL (reservedWord "and" *> pure (PrimBinOp OpAnd))
      , InfixL (reservedWord "or"  *> pure (PrimBinOp OpOr))] ]

lOps :: [[Operator (ParsecT Dec String Identity) Expr]]
lOps = [ [InfixR (symbol ":" >> return listcons)] ]

relation :: ParsecT Dec String Identity (Expr -> Expr -> Expr)
relation = 
          symbol "<=" *> pure (PrimBinOp OpLe)
      <|> symbol ">=" *> pure (PrimBinOp OpGe)
      <|> symbol "<"  *> pure (PrimBinOp OpLt)
      <|> symbol ">"  *> pure (PrimBinOp OpGt)
      <|> symbol "==" *> pure (PrimBinOp OpEq)
      
aTerm :: ParsecT Dec String Identity Expr
aTerm = parens aExpr
    -- <|> varName
    <|> Double <$> try float
    <|> Num <$> integer

bTerm :: ParsecT Dec String Identity Expr
bTerm = parens bExpr
    <|> (reservedWord "true" *> pure (Boolean True))
    <|> (reservedWord "false" *> pure (Boolean False))
    <|> rExpr

lTerm = do
    elems <- brackets $ aExpr `sepBy` comma
    return $ foldr (\x xs -> App (App (Con "cons") x) xs) (Con "nil") elems

rExpr :: ParsecT Dec String Identity Expr
rExpr = do
    a1 <- aExpr
    op <- relation
    a2 <- aExpr
    return $ op a1 a2

assignment :: ParsecT Dec String Identity Expr
assignment = do
    var <- varName
    void $ symbol ":="
    expr <- termParser
    return $ Def (Var var) expr

ifthenelse :: ParsecT Dec String Identity Expr
ifthenelse = do 
    reservedWord "if"
    cond <- termParser
    reservedWord "then"
    stmt1 <- termParser
    reservedWord "else"
    stmt2 <- termParser
    return $ IfThenElse cond stmt1 stmt2

{-lambda = do-}
    {-void $ symbol "\\"-}
    {-pats <- some (try pat)-}
    {-void $ symbol "->"-}
    {-body <- termParser-}
    {-return $ Lam pats body-}

listcons :: Expr -> Expr -> Expr
listcons l r = App (App (Con "cons") l) r
                                 
termParser :: ParsecT Dec String Identity Expr
termParser = assignment <|> try ifthenelse <|> try bExpr <|> aExpr <|> lTerm

--pat = undefined

-------------------------
-- Indentation Parsers --
-------------------------

whereBlock :: Parser (String, [String])
whereBlock = L.indentBlock scn p
    where
        p = do
            header <- pItem
            return (L.IndentMany Nothing (return . (header, )) pLineFold)

pItem :: Parser String
pItem = lexeme $ some (alphaNumChar <|> char '-')

pLineFold :: Parser String
pLineFold = L.lineFold scn $ \sc' ->
        let ps = some (alphaNumChar <|> char '-') `sepBy1` try sc'
        in unwords <$> ps <* scn

indentParser :: Parser (String, [String])
indentParser = whereBlock <* eof

-----------------
-- File Parser --
-----------------

exprParser :: ParsecT Dec String Identity Expr
-- exprParser = termParser <|> try varName <|> constructorName <|> stringLit
exprParser = termParser <|> stringLit

readExpr :: String -> Expr
readExpr input = 
    case parse exprParser "microML" input of
      Left err -> StringLit $ "No match: " ++ show err
      Right res -> res

parseFromFile :: Parsec e String a -> String -> IO (Either (ParseError Char e) a)
parseFromFile p file = runParser p file <$> readFile file
