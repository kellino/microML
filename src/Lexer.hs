module Lexer where

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

reservedWord :: String -> Parser ()
reservedWord w = string w *> notFollowedBy alphaNumChar *> sc

reserved :: [String]
reserved = ["if", "then", "else", "let", "true", "false", "and", "or", "not", "otherwise", "where", "alias", "using", "main", "rec"]

identifier :: Parser String
identifier = lexeme (p >>= check)
    where 
        p = (:) <$> letterChar <*> many alphaNumChar 
        check x = if x `elem` reserved
                     then fail $ show x ++ " is a \ESC[1mreserved word\ESC[0m and cannot be used as an \ESC[1midentifier\ESC[0m" 
                     else return x

contents :: Parser a -> Parser a
contents p = do
    -- void $ try whiteSpace 
    r <- p
    eof
    return r
