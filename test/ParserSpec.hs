module ParserSpec where

import Test.Hspec
import Data.Either (isLeft)

import Lexer.Token
import Parser.Ast
import Parser.Monad (ParserError)
import Parser.Rules (parse)

-- Build a Token with a dummy position
tok :: TokenKind -> Token
tok k = Token k (SourcePosition 0 0)

-- Parse a sequence of TokenKinds
p :: [TokenKind] -> Either ParserError Expr
p = parse . map tok

spec :: Spec
spec = do
    describe "literals" $ do
        it "int" $
            p [TInt 42, TEof] `shouldBe` Right (ELit (LInt 42))
        it "negative int" $
            p [TInt (-7), TEof] `shouldBe` Right (ELit (LInt (-7)))
        it "zero" $
            p [TInt 0, TEof] `shouldBe` Right (ELit (LInt 0))
        it "float" $
            p [TFloat 3.14, TEof] `shouldBe` Right (ELit (LFloat 3.14))
        it "negative float" $
            p [TFloat (-0.5), TEof] `shouldBe` Right (ELit (LFloat (-0.5)))
        it "string" $
            p [TString "hello", TEof] `shouldBe` Right (ELit (LString "hello"))
        it "empty string" $
            p [TString "", TEof] `shouldBe` Right (ELit (LString ""))
        it "char" $
            p [TChar 'a', TEof] `shouldBe` Right (ELit (LChar 'a'))
        it "newline char" $
            p [TChar '\n', TEof] `shouldBe` Right (ELit (LChar '\n'))

    describe "identifier" $ do
        it "simple ident" $
            p [TIdent "foo", TEof] `shouldBe` Right (EIdent "foo")
        it "underscore ident" $
            p [TIdent "_x", TEof] `shouldBe` Right (EIdent "_x")
        it "ident with digits" $
            p [TIdent "f1", TEof] `shouldBe` Right (EIdent "f1")

    describe "application" $ do
        it "f x" $
            p [TIdent "f", TIdent "x", TEof] `shouldBe`
                Right (EApp (EIdent "f") (EIdent "x"))
        it "f x y  (left-associative)" $
            p [TIdent "f", TIdent "x", TIdent "y", TEof] `shouldBe`
                Right (EApp (EApp (EIdent "f") (EIdent "x")) (EIdent "y"))
        it "f x y z  (three args)" $
            p [TIdent "f", TIdent "x", TIdent "y", TIdent "z", TEof] `shouldBe`
                Right (EApp (EApp (EApp (EIdent "f") (EIdent "x")) (EIdent "y")) (EIdent "z"))
        it "applied to int literal" $
            p [TIdent "f", TInt 1, TEof] `shouldBe`
                Right (EApp (EIdent "f") (ELit (LInt 1)))
        it "applied to string literal" $
            p [TIdent "f", TString "hi", TEof] `shouldBe`
                Right (EApp (EIdent "f") (ELit (LString "hi")))
        it "f (g x)  →  f applied to (g x)" $
            p [TIdent "f", TLParen, TIdent "g", TIdent "x", TRParen, TEof] `shouldBe`
                Right (EApp (EIdent "f") (EApp (EIdent "g") (EIdent "x")))
        it "f (g (h x))  →  nested parens" $
            p [ TIdent "f"
              , TLParen, TIdent "g"
              , TLParen, TIdent "h", TIdent "x", TRParen
              , TRParen, TEof ] `shouldBe`
                Right (EApp (EIdent "f")
                       (EApp (EIdent "g") (EApp (EIdent "h") (EIdent "x"))))
        it "literal applied to literal (valid syntactically)" $
            p [TInt 1, TInt 2, TEof] `shouldBe`
                Right (EApp (ELit (LInt 1)) (ELit (LInt 2)))

    describe "lambda" $ do
        it "\\x -> x" $
            p [TBackslash, TIdent "x", TArrow, TIdent "x", TEof] `shouldBe`
                Right (ELam ["x"] (EIdent "x"))
        it "\\x -> 42" $
            p [TBackslash, TIdent "x", TArrow, TInt 42, TEof] `shouldBe`
                Right (ELam ["x"] (ELit (LInt 42)))
        it "two args: \\x y -> x" $
            p [TBackslash, TIdent "x", TIdent "y", TArrow, TIdent "x", TEof] `shouldBe`
                Right (ELam ["x", "y"] (EIdent "x"))
        it "three args: \\x y z -> z" $
            p [TBackslash, TIdent "x", TIdent "y", TIdent "z", TArrow, TIdent "z", TEof] `shouldBe`
                Right (ELam ["x", "y", "z"] (EIdent "z"))
        it "body with application: \\x -> f x" $
            p [TBackslash, TIdent "x", TArrow, TIdent "f", TIdent "x", TEof] `shouldBe`
                Right (ELam ["x"] (EApp (EIdent "f") (EIdent "x")))
        it "nested lambda: \\x -> \\y -> x" $
            p [ TBackslash, TIdent "x", TArrow
              , TBackslash, TIdent "y", TArrow
              , TIdent "x", TEof ] `shouldBe`
                Right (ELam ["x"] (ELam ["y"] (EIdent "x")))
        it "lambda applied to arg: (\\x -> x) 42" $
            p [ TLParen, TBackslash, TIdent "x", TArrow, TIdent "x", TRParen
              , TInt 42, TEof ] `shouldBe`
                Right (EApp (ELam ["x"] (EIdent "x")) (ELit (LInt 42)))

    describe "binary operators" $ do
        it "1 + 2" $
            p [TInt 1, TOp "+", TInt 2, TEof] `shouldBe`
                Right (EApp (EApp (EIdent "+") (ELit (LInt 1))) (ELit (LInt 2)))
        it "1 - 2" $
            p [TInt 1, TOp "-", TInt 2, TEof] `shouldBe`
                Right (EApp (EApp (EIdent "-") (ELit (LInt 1))) (ELit (LInt 2)))
        it "2 * 3" $
            p [TInt 2, TOp "*", TInt 3, TEof] `shouldBe`
                Right (EApp (EApp (EIdent "*") (ELit (LInt 2))) (ELit (LInt 3)))
        it "6 / 2" $
            p [TInt 6, TOp "/", TInt 2, TEof] `shouldBe`
                Right (EApp (EApp (EIdent "/") (ELit (LInt 6))) (ELit (LInt 2)))
        it "precedence: 1 + 2 * 3  →  1 + (2 * 3)" $
            p [TInt 1, TOp "+", TInt 2, TOp "*", TInt 3, TEof] `shouldBe`
                Right (EApp (EApp (EIdent "+") (ELit (LInt 1)))
                            (EApp (EApp (EIdent "*") (ELit (LInt 2))) (ELit (LInt 3))))
        it "precedence: 1 * 2 + 3  →  (1 * 2) + 3" $
            p [TInt 1, TOp "*", TInt 2, TOp "+", TInt 3, TEof] `shouldBe`
                Right (EApp (EApp (EIdent "+")
                            (EApp (EApp (EIdent "*") (ELit (LInt 1))) (ELit (LInt 2))))
                            (ELit (LInt 3)))
        it "left-assoc: 1 - 2 - 3  →  (1 - 2) - 3" $
            p [TInt 1, TOp "-", TInt 2, TOp "-", TInt 3, TEof] `shouldBe`
                Right (EApp (EApp (EIdent "-")
                            (EApp (EApp (EIdent "-") (ELit (LInt 1))) (ELit (LInt 2))))
                            (ELit (LInt 3)))
        it "left-assoc: 1 + 2 + 3  →  (1 + 2) + 3" $
            p [TInt 1, TOp "+", TInt 2, TOp "+", TInt 3, TEof] `shouldBe`
                Right (EApp (EApp (EIdent "+")
                            (EApp (EApp (EIdent "+") (ELit (LInt 1))) (ELit (LInt 2))))
                            (ELit (LInt 3)))
        it "op applied to ident: x + y" $
            p [TIdent "x", TOp "+", TIdent "y", TEof] `shouldBe`
                Right (EApp (EApp (EIdent "+") (EIdent "x")) (EIdent "y"))

    describe "unary (prefix) operators" $ do
        it "-x  →  EApp (EIdent \"-\") (EIdent \"x\")" $
            p [TOp "-", TIdent "x", TEof] `shouldBe`
                Right (EApp (EIdent "-") (EIdent "x"))
        it "-42  →  EApp (EIdent \"-\") (ELit 42)" $
            p [TOp "-", TInt 42, TEof] `shouldBe`
                Right (EApp (EIdent "-") (ELit (LInt 42)))
        it "+x  →  EApp (EIdent \"+\") (EIdent \"x\")" $
            p [TOp "+", TIdent "x", TEof] `shouldBe`
                Right (EApp (EIdent "+") (EIdent "x"))
        it "-x + y  →  (-x) + y  (prefix binds tighter than infix)" $
            p [TOp "-", TIdent "x", TOp "+", TIdent "y", TEof] `shouldBe`
                Right (EApp (EApp (EIdent "+") (EApp (EIdent "-") (EIdent "x"))) (EIdent "y"))
        it "- f x  →  -(f x)  (prefix consumes application)" $
            p [TOp "-", TIdent "f", TIdent "x", TEof] `shouldBe`
                Right (EApp (EIdent "-") (EApp (EIdent "f") (EIdent "x")))

    describe "parentheses" $ do
        it "(42)" $
            p [TLParen, TInt 42, TRParen, TEof] `shouldBe` Right (ELit (LInt 42))
        it "(x)" $
            p [TLParen, TIdent "x", TRParen, TEof] `shouldBe` Right (EIdent "x")
        it "((x))" $
            p [TLParen, TLParen, TIdent "x", TRParen, TRParen, TEof] `shouldBe`
                Right (EIdent "x")
        it "(f x)  →  EApp" $
            p [TLParen, TIdent "f", TIdent "x", TRParen, TEof] `shouldBe`
                Right (EApp (EIdent "f") (EIdent "x"))
        it "(1 + 2) * 3  →  (* (+ 1 2) 3)" $
            p [TLParen, TInt 1, TOp "+", TInt 2, TRParen, TOp "*", TInt 3, TEof] `shouldBe`
                Right (EApp (EApp (EIdent "*")
                            (EApp (EApp (EIdent "+") (ELit (LInt 1))) (ELit (LInt 2))))
                            (ELit (LInt 3)))
        it "1 * (2 + 3)  →  (* 1 (+ 2 3))" $
            p [TInt 1, TOp "*", TLParen, TInt 2, TOp "+", TInt 3, TRParen, TEof] `shouldBe`
                Right (EApp (EApp (EIdent "*") (ELit (LInt 1)))
                            (EApp (EApp (EIdent "+") (ELit (LInt 2))) (ELit (LInt 3))))

    describe "parse errors" $ do
        it "rejects bare EOF" $
            p [TEof] `shouldSatisfy` isLeft
        it "rejects unclosed paren" $
            p [TLParen, TInt 42, TEof] `shouldSatisfy` isLeft
        it "rejects empty parens" $
            p [TLParen, TRParen, TEof] `shouldSatisfy` isLeft
        it "rejects lone operator as full expression" $
            p [TOp "+", TEof] `shouldSatisfy` isLeft
        it "rejects operator with no right operand" $
            p [TInt 1, TOp "+", TEof] `shouldSatisfy` isLeft
        it "rejects lambda with no body" $
            p [TBackslash, TIdent "x", TArrow, TEof] `shouldSatisfy` isLeft
        it "rejects lambda with no args" $
            p [TBackslash, TArrow, TIdent "x", TEof] `shouldSatisfy` isLeft
        it "rejects arrow without backslash" $
            p [TIdent "x", TArrow, TIdent "x", TEof] `shouldSatisfy` isLeft
