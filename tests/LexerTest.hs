{-# LANGUAGE CPP #-}

module LexerTest where



import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.String
import Lexer

lexer :: IO ()
lexer = hspec $ 
    describe "lexer" $ do
        describe "identifier" $ do
            it "parses a single letter" $ 
                parse (identifier :: Parser String) "" "x" `shouldParse` "x"

            it "parses a letter and a prime symbol" $ 
                parse (identifier :: Parser String) "" "x'" `shouldParse` "x'"

            it "parses a string of lowercase letters" $
                parse (identifier :: Parser String) "" "hello" `shouldParse` "hello"

            it "parses a mixture of upper and lowercase letters" $
                parse (identifier :: Parser String) "" "hEllO" `shouldParse` "hEllO"

            it "parses a mixture of letters, numbers and symbols" $
                parse (identifier :: Parser String) "" "test1_" `shouldParse` "test1_"

            it "should not parse a some symbols" $
                parse (identifier :: Parser String) "" `shouldFailOn` "-$£"
    
        describe "charLiteral" $ 
            it "parses a single ascii char" $
                parse (charLiteral :: Parser Char) "" "x" `shouldParse` 'x'
