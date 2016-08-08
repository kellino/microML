-- | A very limited pseudo-markdown parser for pretty printing the help information to the terminal
-- in interactive sessions
--
{-# LANGUAGE OverloadedStrings #-}

module Repl.Help where

import Control.Monad (void)

import Text.Parsec
import Text.Parsec.Text.Lazy
import Text.PrettyPrint

import qualified Data.Text.Lazy as L

data Markdown = 
        Emphasis String
      | Header String
      | Plain String
      | Background String
      | Underline String
      deriving (Eq, Show)

funcChars :: Parser Char
funcChars = oneOf " '?:+<>-=)!.(,;\"\n"

funcName :: Parser String
funcName = do
    void $ string "=="
    st <- many1 $ alphaNum <|> oneOf "?'_"
    void $ string "=="
    return st

comments :: Parser String
comments = do
    void $ string "let" <|> string "--"
    _ <- anyChar `manyTill` newline
    return ""

header :: Parser Markdown
header = do
    void $ many1 $ string "#"
    st <- many1 (alphaNum <|> funcChars) 
    void $ many1 $ string "#"
    return $ Header st

emph :: Parser Markdown
emph = do
    void $ string "**" 
    st <- many1 (alphaNum <|> funcChars)
    void $ string "**"
    return $ Emphasis st

background :: Parser Markdown
background = do
    void $ string "***"
    st <- many1 (alphaNum <|> funcChars)
    void $ string "***"
    return $ Background st

underline :: Parser Markdown
underline = do
    void $ string "__" 
    st <- many1 $ alphaNum <|> funcChars 
    void $ string "__"
    return $ Underline st

plain :: Parser Markdown
plain = do
    st <- many1 (alphaNum <|> funcChars)
    return $ Plain st

helpStyle :: Parser Markdown
helpStyle = header 
        <|> try background
        <|> try emph
        <|> underline
        <|> plain
        <?> "markdown syntax"

type HelpBlock = (String, [Markdown])

helpModl :: Parser HelpBlock
helpModl = do
    void $ string "(*" <* spaces
    name <- spaces *> funcName <* spaces
    helpBlock <- many helpStyle <* spaces
    void $ spaces *> string "*)" <* spaces
    skipMany (try comments)
    return (name, helpBlock)

allHelp :: Parser [HelpBlock]
allHelp = do
    skipMany comments
    sepEndBy1 helpModl (skipMany comments)

parseHelp :: SourceName -> L.Text -> Either ParseError [HelpBlock]
parseHelp = parse allHelp

prettyPrint :: Markdown -> Doc
prettyPrint st = 
    case st of
      (Emphasis s)   -> text "\ESC[1m" <> text s <> text "\ESC[0m"
      (Plain s)      -> text s
      (Header s)     -> text "\ESC[1;31m" <> nest 5 (text s) <> text "\ESC[0m"
      (Background s) -> text "\ESC[1;43;30m" <> text "        "  <> text s <> text "        " <> text "\ESC[0m"
      (Underline s)  -> text "\ESC[4m" <> text s <> text "\ESC[0m"

renderHelp :: [Markdown] -> String
renderHelp = concatMap (render . prettyPrint)
