module LexerSpec where

import Test.Hspec

import Lexer.Token
import Lexer.Rules
import Lexer.Monad (LexerError(..))

spec :: Spec
spec = do
    describe "lexInt" $ do
        it "parses positive integer" $
            tokenize "42" `shouldBe` Right [TInt 42, TEof]
        it "parses negative integer" $
            tokenize "-42" `shouldBe` Right [TInt (-42), TEof]
        it "parses zero" $
            tokenize "0" `shouldBe` Right [TInt 0, TEof]

    describe "lexFloat" $ do
        it "parses float" $
            tokenize "3.14" `shouldBe` Right [TFloat 3.14, TEof]
        it "parses negative float" $
            tokenize "-3.14" `shouldBe` Right [TFloat (-3.14), TEof]

    describe "lexString" $ do
        it "parses string" $
            tokenize "\"hello\"" `shouldBe` Right [TString "hello", TEof]
        it "parses empty string" $
            tokenize "\"\"" `shouldBe` Right [TString "", TEof]

    describe "lexChar" $ do
        it "parses char" $
            tokenize "'a'" `shouldBe` Right [TChar 'a', TEof]
        it "parses escape newline" $
            tokenize "'\\n'" `shouldBe` Right [TChar '\n', TEof]
        it "parses escape tab" $
            tokenize "'\\t'" `shouldBe` Right [TChar '\t', TEof]

    describe "lexIdent" $ do
        it "parses identifier" $
            tokenize "foo" `shouldBe` Right [TIdent "foo", TEof]
        it "parses identifier with underscore" $
            tokenize "_bar" `shouldBe` Right [TIdent "_bar", TEof]
        it "parses identifier with digits" $
            tokenize "foo123" `shouldBe` Right [TIdent "foo123", TEof]

    describe "lexOp" $ do
        it "parses operator" $
            tokenize "+" `shouldBe` Right [TOp "+", TEof]
        it "parses arrow" $
            tokenize "->" `shouldBe` Right [TArrow, TEof]
        it "parses double colon" $
            tokenize "::" `shouldBe` Right [TDoubleColon, TEof]
        it "parses eq" $
            tokenize "=" `shouldBe` Right [TEq, TEof]

    describe "lexPunct" $ do
        it "parses parens" $
            tokenize "()" `shouldBe` Right [TLParen, TRParen, TEof]
        it "parses brackets" $
            tokenize "[]" `shouldBe` Right [TLBracket, TRBracket, TEof]

    describe "mixed" $ do
        it "parses let expression" $
            tokenize "let x = 42" `shouldBe` Right [TIdent "let", TIdent "x", TEq, TInt 42, TEof]
        it "parses function type" $
            tokenize "a -> b" `shouldBe` Right [TIdent "a", TArrow, TIdent "b", TEof]
        it "skips whitespace" $
            tokenize "  42  " `shouldBe` Right [TInt 42, TEof]

    describe "errors" $ do
        it "fails on unexpected char" $
            tokenize "%" `shouldBe` Left (LexerError "unexpected character")
