{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Parser (
        exprParser
      , parseFromFile
      , parseProg
      , readExpr
        ) where

import Syntax
import Control.Monad (void)
import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String
import Text.Megaparsec.Expr

import Data.Functor.Identity (Identity)
import Data.Char (isLower, isUpper)

type MLParser = ParsecT Dec String Identity
type RawData t e = [Either (ParseError t e) Expr]

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

-----------------------------
-- SYMBOLS & ATOMIC VALUES --
-----------------------------

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

-- two number types
integer = lexeme L.integer
float = lexeme L.float

-- symbol list
symbol = L.symbol sc
charLiteral = lexeme L.charLiteral
parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")
comma = symbol ","
arrow = symbol "->"
isA = symbol "::="
hasType = symbol "::"
whiteSpace = lexeme spaceChar 

reservedWord :: String -> MLParser ()
reservedWord w = string w *> notFollowedBy alphaNumChar *> sc

-- a list of reserved words, not available for use as identifiers
-- use Miranda style where blocks rather than let - in expressions. 
reserved :: [String]
reserved = ["if", "then", "else", "true", "false", "and", "or", "not", "otherwise", "where", "alias"]

identifier :: Parser String
identifier = lexeme (p >>= check)
    where 
        p = (:) <$> letterChar <*> many alphaNumChar
        check x = if x `elem` reserved
                     then fail $ promptRed ++ show x ++ promptReset ++ " is a \ESC[1mreserved word\ESC[0m and cannot be used as an \ESC[1midentifier\ESC[0m" 
                     else return x

charLit :: MLParser Expr
charLit = do
    void $ symbol "'" 
    c <- L.charLiteral
    void $ symbol "'"
    return $ Char c

stringLit :: MLParser Expr
stringLit = strLit >>= \str -> return $ StringLit str
    where
        strLit = char '"' >> manyTill charLiteral (char '"')

constructorName :: MLParser String
constructorName = do name@(n:_) <- identifier
                     if isUpper n 
                        then return name
                        else fail $ promptBold ++ "a constructor must begin with a capital letter" ++ promptReset 

varName :: MLParser String
varName = do name@(n:_) <- identifier
             if isLower n 
                then return name
                else fail $ promptBold ++ "a variable must start with a lowercase letter" ++ promptReset

------------------------
-- Expression Parsers -- 
------------------------

aExpr, bExpr :: MLParser Expr
-- expression parser for arithmetic
aExpr = makeExprParser aTerm arithOps
-- expression parser for boolean operations
bExpr = makeExprParser bTerm boolOps
-- expression parser for list operations
lExpr = makeExprParser lTerm lOps
-- expression parser for relational operations
rExpr = makeExprParser rTerm relationOps

arithOps :: [[Operator MLParser Expr]]
arithOps =
    [ [ Prefix (symbol "-" >> return Neg) ]
     ,[ InfixL (symbol "*" *> pure (PrimBinOp OpMul))
      , InfixL (symbol "-" *> pure (PrimBinOp OpSub))
      , InfixL (symbol "/" *> pure (PrimBinOp OpDiv))
      , InfixL (symbol "%" *> pure (PrimBinOp OpMod))
      , InfixL (symbol "+" *> pure (PrimBinOp OpAdd))
      , InfixL (symbol "^" *> pure (PrimBinOp OpExp))] ]

boolOps :: [[Operator MLParser Expr]]
boolOps =
    [ [ Prefix (reservedWord "not" *> pure Not) ]
     ,[ InfixL (reservedWord "and" *> pure (PrimBinOp OpAnd))
      , InfixL (reservedWord "or"  *> pure (PrimBinOp OpOr))] ]

lOps :: [[Operator MLParser Expr]]
lOps = [ [InfixR (symbol ":" >> return listcons)] ]

relationOps :: [[Operator MLParser Expr]]
relationOps = 
    [ [ InfixL (symbol "<=" *> pure (PrimBinOp OpLe))
      , InfixL (symbol ">=" *> pure (PrimBinOp OpGe))
      , InfixL (symbol "<"  *> pure (PrimBinOp OpLt))
      , InfixL (symbol ">"  *> pure (PrimBinOp OpGt))
      , InfixL (symbol "==" *> pure (PrimBinOp OpEq))]]

aTerm :: MLParser Expr
aTerm = parens aExpr
    <|> Var <$> varName
    <|> Double <$> try float
    <|> Number <$> integer

bTerm :: MLParser Expr
bTerm = parens bExpr
    <|> (reservedWord "true" *> pure (Boolean True))
    <|> (reservedWord "false" *> pure (Boolean False))

lTerm = do
    elems <- brackets $ termParser `sepBy` comma
    return $ foldr (\x xs -> App (App (Con "cons") x) xs) (Con "nil") elems

rTerm :: MLParser Expr
rTerm = parens rExpr
    <|> try bTerm
    <|> aTerm

assignment :: MLParser Expr
assignment = do
    var <- varName
    void $ symbol ":="
    expr <- termParser
    return $ Def (Var var) expr

ifthenelse :: MLParser Expr
ifthenelse = do 
    reservedWord "if"
    cond <- termParser
    reservedWord "then"
    stmt1 <- termParser
    reservedWord "else"
    stmt2 <- termParser
    return $ IfThenElse cond stmt1 stmt2

listcons :: Expr -> Expr -> Expr
-- listcons l r is clearer, but ghc-mod is really bugging me to do the η reduction, so
-- I've done it just to shut it up.
listcons l = App (App (Con "cons") l)

tuple :: MLParser Expr
tuple = do
    void $ symbol "("
    itms <- some (aExpr <|> stringLit) `sepBy` comma
    void $ symbol ")"
    return $ Tuple (concat itms)

termParser :: MLParser Expr
termParser = 
            try assignment 
        <|> try rExpr
        <|> try ifthenelse 
        <|> try bExpr 
        <|> aExpr 
        <|> lTerm 
        <|> stringLit 
        <|> charLit 
        <|> lambda
        <|> parens termParser

--------------------
-- LAMBDA PARSER --
--------------------

{- by placing lambda expressions within a different constructor family, it is easier to "lift" them
   into the global scope as they can be easily identified within the program's parse tree -}

lambda :: MLParser Expr
lambda = do
    void $ symbol "\\"
    pats <- some pat
    void arrow
    body <- termParser
    return $ Lam pats body

pat :: MLParser Pat
pat = makeExprParser pTerms pTable <?> "pattern"
    where pTable = [[ InfixR (symbol ":" >> return listcons')]]
          
          pTerms = try varPat <|> try conPat <|> list <|> wildcard <|> intPat <|> boolPat <|> parens pat

          conPat = do
              con <- constructorName
              pats <- some pat
              return $ PApp con pats

          varPat = PVar <$> varName 

          wildcard = do 
              void $ symbol "_"
              return Wildcard 

          intPat = IntPat . fromInteger <$> integer

          boolPat = BoolPat <$> boolLit
              where boolLit = (reservedWord "true" >> return True)
                          <|> (reservedWord "false" >> return False)

          listcons' hd rst = PApp "Cons" [hd, rst]

          list = do
              pats <- brackets $ pat `sepBy` comma
              return $ foldr listcons' (PApp "Nil" []) pats

---------------------------
-- TYPE SIGNATURE PARSER --
---------------------------

typeSig :: MLParser Type
typeSig = makeExprParser tyTerms tyTable <?> "type signature"
    where tyTable = [[ InfixL (whiteSpace >> return TypeApp)]]

          tyTerms = try primitiveType 
                <|> try typeVar 
                <|> dataType 
                <|> listType 
                <|> curriedType

          listType = TypeList <$> brackets typeSig
          typeVar = TypeVar <$> varName
          dataType = TypeData <$> constructorName
          primitiveType = (reservedWord "Number" >> return (TypePrimitive TypeInt))
                      <|> (reservedWord "Double" >> return (TypePrimitive TypeDouble))
                      <|> (reservedWord "Bool" >> return (TypePrimitive TypeBool))
          curriedType = do
              void $ symbol "("
              expr <- some typeSig `sepBy` arrow
              void $ symbol ")"
              return $ TypeCurry (concat expr)

typeParser :: MLParser Expr
typeParser = do
    name <- TypeVar <$> varName
    void hasType
    types <- some typeSig `sepBy` arrow
    return $ TypeSig name (concat types)

----------------
-- ADT Parser --
----------------

newTypes = try typeAlias <|> try adtDecl <|> typeParser <?> "type declaration"

typeAlias :: MLParser Expr
typeAlias = do
    void $ reservedWord "alias"
    con <- constructorName
    params <- option "" varName
    void $ symbol ":="
    ty <- constructorName
    return $ TypeDec $ TypeAlias con params ty

adtDecl :: MLParser Expr
adtDecl = do
    newCon <- Con <$> constructorName
    param <- Var <$> option "" varName 
    void $ symbol "::=" 
    cons <- many dataCon `sepBy` symbol "|"
    return $ ADT newCon param (concat cons)

dataCon :: MLParser Expr
dataCon = parens dataCon' <|> dataCon'

dataCon' :: MLParser Expr
dataCon' = do
    newCon <- Con <$> constructorName
    param <- Var <$> option "" varName
    return $ DataCon newCon param

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

exprParser :: MLParser Expr
exprParser = try tuple <|> try newTypes <|> termParser

readExpr :: String -> Either (ParseError Char Dec) Expr
readExpr = parse exprParser "microML"

{-parseProg :: MLParser [Expr]-}
{-parseProg = between scn eof (exprParser `sepEndBy` scn)-}

parseProg :: Parser (RawData Char Dec)
parseProg = between scn eof (e `sepEndBy` scn)
    where e = withRecovery recover (Right <$> exprParser)
          recover err = Left err <$ manyTill anyChar eol

parseFromFile :: Parsec e String a -> String -> IO (Either (ParseError Char e) a)
parseFromFile p file = runParser p file <$> readFile file
