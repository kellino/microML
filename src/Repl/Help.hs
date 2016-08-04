-- | A very limitied psuedo-markdown parser for pretty printing the help information to the terminal
-- in interactive sessions

{-# LANGUAGE OverloadedStrings #-}

module Repl.Help where

import Control.Monad

import Text.Parsec
import Text.Parsec.String

data Markdown = 
        Emphasis String
      | Header String
      | Plain String
      | BackGround String
      deriving Show

funcChars :: Parser Char
funcChars = oneOf " :+<>-=)(\n"

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
    return $ BackGround st

emph2 :: Parser Markdown
emph2 = do
    void $ string "__" 
    st <- many1 $ alphaNum <|> funcChars 
    void $ string "__"
    return $ Emphasis st

plain :: Parser Markdown
plain = do
    st <- many1 (alphaNum <|> funcChars)
    return $ Plain st

helpStyle :: Parser Markdown
helpStyle = header 
        <|> try background
        <|> emph
        <|> emph2
        <|> plain
        <?> "markdown syntax"

helpModl :: Parser [Markdown]
helpModl = many helpStyle 

parseHelp :: String -> Either ParseError [Markdown]
parseHelp = parse helpModl "from help" 

prettyPrint :: Markdown -> String
prettyPrint st = 
    case st of
      (Emphasis s) -> "\ESC[1m" ++ s ++ "\ESC[0m"
      (Plain s) -> s
      (Header s) -> "\ESC[1;31m" ++ s ++ "\ESC[0m"
      (BackGround s) -> "\ESC[43;30m" ++ s ++ "\ESC[0m"

printHelp :: String -> String
printHelp help = 
    case parseHelp help of
      Left e -> error $ show e
      Right r -> concatMap prettyPrint r
