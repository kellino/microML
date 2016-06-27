module Language.Lexer where

import Control.Monad (void)
import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String


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
operator = lexeme $ oneOf ".$!"

reservedWord :: String -> Parser ()
reservedWord w = string w *> notFollowedBy alphaNumChar *> sc

reserved :: [String]
reserved = ["if", "then", "else", "let", "true", "false", "and", "or", "not", 
            "otherwise", "where", "alias", "using", "infixL", "infixR", "infix", "postfix", "prefix"]

identifier :: Parser String
identifier = lexeme (p >>= check)
    where 
        p = (:) <$> letterChar <*> many (alphaNumChar <|> oneOf "'?_")
        check x = if x `elem` reserved
                     then fail $ show x ++ " is a reserved word and cannot be used as an identifier" 
                     else return x

contents :: Parser a -> Parser a
contents p = do
    _ <- optional scn
    r <- p
    eof
    return r
