module LexerSpec where

import Test.Hspec
import Data.Either (isLeft)

import Lexer.Token
import Lexer.Rules
import Lexer.Monad (LexerError)

-- Strip positions so we can compare TokenKind lists
tokenizeKinds :: String -> Either LexerError [TokenKind]
tokenizeKinds s = fmap (map tokenKind) (tokenize s)

spec :: Spec
spec = do
    describe "lexInt" $ do
        it "parses positive integer" $
            tokenizeKinds "42" `shouldBe` Right [TInt 42, TEof]
        it "parses negative integer" $
            tokenizeKinds "-42" `shouldBe` Right [TInt (-42), TEof]
        it "parses zero" $
            tokenizeKinds "0" `shouldBe` Right [TInt 0, TEof]

    describe "lexFloat" $ do
        it "parses float" $
            tokenizeKinds "3.14" `shouldBe` Right [TFloat 3.14, TEof]
        it "parses negative float" $
            tokenizeKinds "-3.14" `shouldBe` Right [TFloat (-3.14), TEof]

    describe "lexString" $ do
        it "parses string" $
            tokenizeKinds "\"hello\"" `shouldBe` Right [TString "hello", TEof]
        it "parses empty string" $
            tokenizeKinds "\"\"" `shouldBe` Right [TString "", TEof]

    describe "lexChar" $ do
        it "parses char" $
            tokenizeKinds "'a'" `shouldBe` Right [TChar 'a', TEof]
        it "parses escape newline" $
            tokenizeKinds "'\\n'" `shouldBe` Right [TChar '\n', TEof]
        it "parses escape tab" $
            tokenizeKinds "'\\t'" `shouldBe` Right [TChar '\t', TEof]

    describe "lexIdent" $ do
        it "parses identifier" $
            tokenizeKinds "foo" `shouldBe` Right [TIdent "foo", TEof]
        it "parses identifier with underscore" $
            tokenizeKinds "_bar" `shouldBe` Right [TIdent "_bar", TEof]
        it "parses identifier with digits" $
            tokenizeKinds "foo123" `shouldBe` Right [TIdent "foo123", TEof]

    describe "lexOp" $ do
        it "parses operator" $
            tokenizeKinds "+" `shouldBe` Right [TOp "+", TEof]
        it "parses arrow" $
            tokenizeKinds "->" `shouldBe` Right [TArrow, TEof]
        it "parses backslash" $
            tokenizeKinds "\\" `shouldBe` Right [TBackslash, TEof]

    describe "lexPunct" $ do
        it "parses parens" $
            tokenizeKinds "()" `shouldBe` Right [TLParen, TRParen, TEof]

    describe "mixed" $ do
        it "tokenizes lambda" $
            tokenizeKinds "\\x -> x" `shouldBe`
                Right [TBackslash, TIdent "x", TArrow, TIdent "x", TEof]
        it "tokenizes arrow expression" $
            tokenizeKinds "a -> b" `shouldBe` Right [TIdent "a", TArrow, TIdent "b", TEof]
        it "skips whitespace" $
            tokenizeKinds "  42  " `shouldBe` Right [TInt 42, TEof]

    describe "errors" $ do
        it "fails on unexpected char" $
            tokenizeKinds "%" `shouldSatisfy` isLeft
