module Language.Lexer where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity

reservedNames :: [String]
reservedNames = ["if", "then", "else", "let", "in", "true", "false", "infixL"]

reservedOps :: [String]
reservedOps = ["->", "\\", "+", "*", "-", "="]

lexer :: Tok.GenTokenParser String () Identity
lexer = Tok.makeTokenParser Tok.LanguageDef
    { Tok.commentStart      = "(*"
    , Tok.commentEnd        = "*)"
      , Tok.commentLine     = "**"
      , Tok.nestedComments  = True
      , Tok.identStart      = letter
      , Tok.identLetter     = alphaNum <|> oneOf "_'"
      , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
      , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
      , Tok.reservedNames   = reservedNames
      , Tok.reservedOpNames = reservedOps
      , Tok.caseSensitive   = True
    }

reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
identifier = Tok.identifier lexer
parens = Tok.parens lexer
semiSep = Tok.semiSep lexer
semi = Tok.semi lexer
braces = Tok.braces lexer
brackets = Tok.brackets lexer
chr = Tok.charLiteral lexer
str = Tok.stringLiteral lexer
operator = Tok.operator lexer
integer = Tok.integer lexer
double = Tok.float lexer

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r
