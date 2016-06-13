{-# LANGUAGE TupleSections #-}

module Parser where

import Syntax
import Control.Monad (void)
import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String
import Text.Megaparsec.Expr

import Data.Functor.Identity (Identity)
import Data.Scientific (Scientific)
import Data.Char (isLower, isUpper)

promptBold :: String
promptBold = "\ESC[1m"

promptUnBold :: String
promptUnBold = "\ESC[0m"

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

blockComment :: Parser ()
blockComment = L.skipBlockComment "(*" "*)"

-- space consumer for newlines
scn :: Parser ()
scn = L.space (void spaceChar) lineComment blockComment

-- space consumer for folds and blocks, i.e ignores newline characters
sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineComment blockComment

-------------------------------
-- Symbols and Protected Ops --
-------------------------------

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
colon = symbol ":"

reservedWord :: String -> ParsecT Dec String Identity ()
reservedWord w = string w *> notFollowedBy alphaNumChar *> sc

reserved :: [String]
reserved = ["if", "then", "else", "true", "false", "let", "in", "and", "or", "not"]

identifier :: Parser String
identifier = lexeme (p >>= check)
    where 
        p = (:) <$> letterChar <*> many alphaNumChar
        check x = if x `elem` reserved
                     then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                     else return x

parseString :: ParsecT Dec String Identity Expr
parseString = stringLit >>= \str -> return $ StringLit str
    where
        stringLit = char '"' >> manyTill charLiteral (char '"')

-- generic number parser
number :: Parser Data.Scientific.Scientific
number = lexeme L.number

constructorName :: ParsecT Dec String Identity Expr
constructorName = do name@(n:_) <- identifier
                     if isUpper n 
                        then return (Con name)
                        else fail $ promptBold ++ "a constructor must begin with a capital letter" ++ promptUnBold 

varName :: ParsecT Dec String Identity Expr
varName = do name@(n:_) <- identifier
             if isLower n
                then return (Var name)
                else fail $ promptBold ++ "a variable must start with a lowercase letter" ++ promptUnBold

------------------------
-- Expression Parsers -- 
------------------------

aExpr, bExpr :: ParsecT Dec String Identity Expr
aExpr = makeExprParser aTerm arithOps
bExpr = makeExprParser bTerm boolOps

arithOps :: [[Operator (ParsecT Dec String Identity) Expr]]
arithOps =
    [ [ InfixL (symbol "*" *> pure (PrimBinOp OpMul))
      , InfixL (symbol "-" *> pure (PrimBinOp OpSub))
      , InfixL (symbol "/" *> pure (PrimBinOp OpDiv))
      , InfixL (symbol "%" *> pure (PrimBinOp OpMod))
      , InfixL (symbol "+" *> pure (PrimBinOp OpAdd))] ]

boolOps :: [[Operator (ParsecT Dec String Identity) Expr]]
boolOps =
    [ [ Prefix (reservedWord "not" *> pure Not) ]
     ,[ InfixL (reservedWord "and" *> pure (PrimBinOp OpAnd))
      , InfixL (reservedWord "or"  *> pure (PrimBinOp OpOr))] ]

relation :: ParsecT Dec String Identity (Expr -> Expr -> Expr)
relation = 
          symbol "<"  *> pure (PrimBinOp OpLt)
      <|> symbol ">"  *> pure (PrimBinOp OpGt)
      <|> symbol "<=" *> pure (PrimBinOp OpLe)
      <|> symbol ">=" *> pure (PrimBinOp OpLt)
      <|> symbol "=?" *> pure (PrimBinOp OpEq) 
      
aTerm :: ParsecT Dec String Identity Expr
aTerm = parens aExpr
    <|> varName
    <|> Double <$> try float
    <|> Num <$> integer

bTerm :: ParsecT Dec String Identity Expr
bTerm = parens bExpr
    <|> (reservedWord "true" *> pure (Boolean True))
    <|> (reservedWord "false" *> pure (Boolean False))
    <|> rExpr

rExpr = do
    a1 <- aExpr
    op <- relation
    a2 <- aExpr
    return $ op a1 a2


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
exprParser = try bExpr <|> aExpr <|> try varName <|> constructorName <|> parseString

readExpr :: String -> Expr
readExpr input = 
    case parse exprParser "microML" input of
      Left err -> StringLit $ "No match: " ++ show err
      Right res -> res

parseFromFile :: Parsec e String a -> String -> IO (Either (ParseError Char e) a)
parseFromFile p file = runParser p file <$> readFile file
