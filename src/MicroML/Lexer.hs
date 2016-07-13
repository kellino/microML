module MicroML.Lexer where

import Text.Parsec
import Text.Parsec.Text.Lazy
import qualified Data.Text.Lazy as L
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

import Data.Functor.Identity

type Op a = Ex.Operator L.Text () Identity a
type Operators a = Ex.OperatorTable L.Text () Identity a

reservedNames :: [String]
reservedNames = [ "let", "in", "if", "then", "else", "case", "of", "and", 
                  "or", "not", "xor", "head", "tail", "to", "where", "true", "false" ]

reservedOps :: [String]
reservedOps = [ "->", "\\", "+", "*", "-", "=", "==", "%", "^", "/", "<=", ">=", ">", "<", ":", "_", "++" ]

lexer :: Tok.GenTokenParser L.Text () Identity
lexer = Tok.makeTokenParser Tok.LanguageDef
  { Tok.commentStart    = "(*"
  , Tok.commentEnd      = "*)"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'?"
  , Tok.opStart         = oneOf ":!$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = reservedNames
  , Tok.reservedOpNames = reservedOps
  , Tok.caseSensitive   = True
  }

reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
identifier = Tok.identifier lexer
parens     = Tok.parens lexer
brackets   = Tok.brackets lexer
braces     = Tok.braces lexer
comma      = Tok.commaSep lexer
semi       = Tok.semi lexer
integer    = Tok.integer lexer
chr        = Tok.charLiteral lexer
str        = Tok.stringLiteral lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r
