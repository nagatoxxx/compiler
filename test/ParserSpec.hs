module ParserSpec where

import Test.Hspec
import Data.Either (isLeft)

import Lexer.Token
import Common.SourcePosition
import Parser.Ast
import Parser.Monad (ParserError)
import Parser.Rules (parse)

tok :: TokenKind -> Token
tok k = k @@ mempty

-- Strip all source locations so tests don't depend on position info
stripF :: ExprF -> ExprF
stripF (EIdent i)   = EIdent (pure (value i))
stripF (ELit l)     = ELit (pure (value l))
stripF (EApp f x)   = EApp (pure (stripF (value f))) (pure (stripF (value x)))
stripF (ELam v e)   = ELam (pure (value v)) (pure (stripF (value e)))

p :: [TokenKind] -> Either ParserError ExprF
p = fmap (stripF . value) . parse . map tok

-- Smart constructors for test expectations
lit :: LiteralF -> ExprF
lit = ELit . pure

eident :: String -> ExprF
eident = EIdent . pure

eapp :: ExprF -> ExprF -> ExprF
eapp f x = EApp (pure f) (pure x)

elam :: String -> ExprF -> ExprF
elam v e = ELam (pure v) (pure e)

spec :: Spec
spec = do
    describe "literals" $ do
        it "int" $
            p [TInt 42, TEof] `shouldBe` Right (lit (LInt 42))
        it "negative int" $
            p [TInt (-7), TEof] `shouldBe` Right (lit (LInt (-7)))
        it "zero" $
            p [TInt 0, TEof] `shouldBe` Right (lit (LInt 0))
        it "float" $
            p [TFloat 3.14, TEof] `shouldBe` Right (lit (LFloat 3.14))
        it "negative float" $
            p [TFloat (-0.5), TEof] `shouldBe` Right (lit (LFloat (-0.5)))
        it "string" $
            p [TString "hello", TEof] `shouldBe` Right (lit (LString "hello"))
        it "empty string" $
            p [TString "", TEof] `shouldBe` Right (lit (LString ""))
        it "char" $
            p [TChar 'a', TEof] `shouldBe` Right (lit (LChar 'a'))
        it "newline char" $
            p [TChar '\n', TEof] `shouldBe` Right (lit (LChar '\n'))

    describe "identifier" $ do
        it "simple ident" $
            p [TIdent "foo", TEof] `shouldBe` Right (eident "foo")
        it "underscore ident" $
            p [TIdent "_x", TEof] `shouldBe` Right (eident "_x")
        it "ident with digits" $
            p [TIdent "f1", TEof] `shouldBe` Right (eident "f1")

    describe "application" $ do
        it "f x" $
            p [TIdent "f", TIdent "x", TEof] `shouldBe`
                Right (eapp (eident "f") (eident "x"))
        it "f x y  (left-associative)" $
            p [TIdent "f", TIdent "x", TIdent "y", TEof] `shouldBe`
                Right (eapp (eapp (eident "f") (eident "x")) (eident "y"))
        it "f x y z  (three args)" $
            p [TIdent "f", TIdent "x", TIdent "y", TIdent "z", TEof] `shouldBe`
                Right (eapp (eapp (eapp (eident "f") (eident "x")) (eident "y")) (eident "z"))
        it "applied to int literal" $
            p [TIdent "f", TInt 1, TEof] `shouldBe`
                Right (eapp (eident "f") (lit (LInt 1)))
        it "applied to string literal" $
            p [TIdent "f", TString "hi", TEof] `shouldBe`
                Right (eapp (eident "f") (lit (LString "hi")))
        it "f (g x)  →  f applied to (g x)" $
            p [TIdent "f", TLParen, TIdent "g", TIdent "x", TRParen, TEof] `shouldBe`
                Right (eapp (eident "f") (eapp (eident "g") (eident "x")))
        it "f (g (h x))  →  nested parens" $
            p [ TIdent "f"
              , TLParen, TIdent "g"
              , TLParen, TIdent "h", TIdent "x", TRParen
              , TRParen, TEof ] `shouldBe`
                Right (eapp (eident "f")
                       (eapp (eident "g") (eapp (eident "h") (eident "x"))))
        it "literal applied to literal (valid syntactically)" $
            p [TInt 1, TInt 2, TEof] `shouldBe`
                Right (eapp (lit (LInt 1)) (lit (LInt 2)))

    describe "lambda" $ do
        it "\\x.x" $
            p [TBackslash, TIdent "x", TDot, TIdent "x", TEof] `shouldBe`
                Right (elam "x" (eident "x"))
        it "\\x.42" $
            p [TBackslash, TIdent "x", TDot, TInt 42, TEof] `shouldBe`
                Right (elam "x" (lit (LInt 42)))
        it "two args: \\x y.x" $
            p [TBackslash, TIdent "x", TIdent "y", TDot, TIdent "x", TEof] `shouldBe`
                Right (elam "x" (elam "y" (eident "x")))
        it "three args: \\x y z.z" $
            p [TBackslash, TIdent "x", TIdent "y", TIdent "z", TDot, TIdent "z", TEof] `shouldBe`
                Right (elam "x" (elam "y" (elam "z" (eident "z"))))
        it "body with application: \\x.f x" $
            p [TBackslash, TIdent "x", TDot, TIdent "f", TIdent "x", TEof] `shouldBe`
                Right (elam "x" (eapp (eident "f") (eident "x")))
        it "nested lambda: \\x.\\y.x" $
            p [ TBackslash, TIdent "x", TDot
              , TBackslash, TIdent "y", TDot
              , TIdent "x", TEof ] `shouldBe`
                Right (elam "x" (elam "y" (eident "x")))
        it "lambda applied to arg: (\\x.x) 42" $
            p [ TLParen, TBackslash, TIdent "x", TDot, TIdent "x", TRParen
              , TInt 42, TEof ] `shouldBe`
                Right (eapp (elam "x" (eident "x")) (lit (LInt 42)))

    describe "binary operators" $ do
        it "1 + 2" $
            p [TInt 1, TOp "+", TInt 2, TEof] `shouldBe`
                Right (eapp (eapp (eident "+") (lit (LInt 1))) (lit (LInt 2)))
        it "1 - 2" $
            p [TInt 1, TOp "-", TInt 2, TEof] `shouldBe`
                Right (eapp (eapp (eident "-") (lit (LInt 1))) (lit (LInt 2)))
        it "2 * 3" $
            p [TInt 2, TOp "*", TInt 3, TEof] `shouldBe`
                Right (eapp (eapp (eident "*") (lit (LInt 2))) (lit (LInt 3)))
        it "6 / 2" $
            p [TInt 6, TOp "/", TInt 2, TEof] `shouldBe`
                Right (eapp (eapp (eident "/") (lit (LInt 6))) (lit (LInt 2)))
        it "precedence: 1 + 2 * 3  →  1 + (2 * 3)" $
            p [TInt 1, TOp "+", TInt 2, TOp "*", TInt 3, TEof] `shouldBe`
                Right (eapp (eapp (eident "+") (lit (LInt 1)))
                            (eapp (eapp (eident "*") (lit (LInt 2))) (lit (LInt 3))))
        it "precedence: 1 * 2 + 3  →  (1 * 2) + 3" $
            p [TInt 1, TOp "*", TInt 2, TOp "+", TInt 3, TEof] `shouldBe`
                Right (eapp (eapp (eident "+")
                            (eapp (eapp (eident "*") (lit (LInt 1))) (lit (LInt 2))))
                            (lit (LInt 3)))
        it "left-assoc: 1 - 2 - 3  →  (1 - 2) - 3" $
            p [TInt 1, TOp "-", TInt 2, TOp "-", TInt 3, TEof] `shouldBe`
                Right (eapp (eapp (eident "-")
                            (eapp (eapp (eident "-") (lit (LInt 1))) (lit (LInt 2))))
                            (lit (LInt 3)))
        it "left-assoc: 1 + 2 + 3  →  (1 + 2) + 3" $
            p [TInt 1, TOp "+", TInt 2, TOp "+", TInt 3, TEof] `shouldBe`
                Right (eapp (eapp (eident "+")
                            (eapp (eapp (eident "+") (lit (LInt 1))) (lit (LInt 2))))
                            (lit (LInt 3)))
        it "op applied to ident: x + y" $
            p [TIdent "x", TOp "+", TIdent "y", TEof] `shouldBe`
                Right (eapp (eapp (eident "+") (eident "x")) (eident "y"))

    describe "unary (prefix) operators" $ do
        it "-x  →  EApp (EIdent \"-\") (EIdent \"x\")" $
            p [TOp "-", TIdent "x", TEof] `shouldBe`
                Right (eapp (eident "-") (eident "x"))
        it "-42  →  EApp (EIdent \"-\") (ELit 42)" $
            p [TOp "-", TInt 42, TEof] `shouldBe`
                Right (eapp (eident "-") (lit (LInt 42)))
        it "+x  →  EApp (EIdent \"+\") (EIdent \"x\")" $
            p [TOp "+", TIdent "x", TEof] `shouldBe`
                Right (eapp (eident "+") (eident "x"))
        it "-x + y  →  (-x) + y  (prefix binds tighter than infix)" $
            p [TOp "-", TIdent "x", TOp "+", TIdent "y", TEof] `shouldBe`
                Right (eapp (eapp (eident "+") (eapp (eident "-") (eident "x"))) (eident "y"))
        it "- f x  →  -(f x)  (prefix consumes application)" $
            p [TOp "-", TIdent "f", TIdent "x", TEof] `shouldBe`
                Right (eapp (eident "-") (eapp (eident "f") (eident "x")))

    describe "parentheses" $ do
        it "(42)" $
            p [TLParen, TInt 42, TRParen, TEof] `shouldBe` Right (lit (LInt 42))
        it "(x)" $
            p [TLParen, TIdent "x", TRParen, TEof] `shouldBe` Right (eident "x")
        it "((x))" $
            p [TLParen, TLParen, TIdent "x", TRParen, TRParen, TEof] `shouldBe`
                Right (eident "x")
        it "(f x)  →  EApp" $
            p [TLParen, TIdent "f", TIdent "x", TRParen, TEof] `shouldBe`
                Right (eapp (eident "f") (eident "x"))
        it "(1 + 2) * 3  →  (* (+ 1 2) 3)" $
            p [TLParen, TInt 1, TOp "+", TInt 2, TRParen, TOp "*", TInt 3, TEof] `shouldBe`
                Right (eapp (eapp (eident "*")
                            (eapp (eapp (eident "+") (lit (LInt 1))) (lit (LInt 2))))
                            (lit (LInt 3)))
        it "1 * (2 + 3)  →  (* 1 (+ 2 3))" $
            p [TInt 1, TOp "*", TLParen, TInt 2, TOp "+", TInt 3, TRParen, TEof] `shouldBe`
                Right (eapp (eapp (eident "*") (lit (LInt 1)))
                            (eapp (eapp (eident "+") (lit (LInt 2))) (lit (LInt 3))))

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
            p [TBackslash, TIdent "x", TDot, TEof] `shouldSatisfy` isLeft
        it "rejects lambda with no args" $
            p [TBackslash, TDot, TIdent "x", TEof] `shouldSatisfy` isLeft
        it "rejects arrow without backslash" $
            p [TIdent "x", TArrow, TIdent "x", TEof] `shouldSatisfy` isLeft
