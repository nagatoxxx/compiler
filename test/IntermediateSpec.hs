module IntermediateSpec where

import Test.Hspec

import Parser.Ast
import Intermediate.Serialization (serialize)

-- Helpers: build Expr nodes with no source location
sid :: String -> Expr
sid n = pure (EIdent (pure n))

slit :: LiteralF -> Expr
slit l = pure (ELit (pure l))

sapp :: Expr -> Expr -> Expr
sapp f x = pure (EApp f x)

slam :: String -> Expr -> Expr
slam v b = pure (ELam (pure v) b)

spec :: Spec
spec = do
    describe "serialize EIdent" $ do
        it "simple name" $
            serialize (sid "x") `shouldBe` "(id x)"
        it "underscore name" $
            serialize (sid "_foo") `shouldBe` "(id _foo)"
        it "name with digits" $
            serialize (sid "f1") `shouldBe` "(id f1)"

    describe "serialize ELit" $ do
        it "int" $
            serialize (slit (LInt 42)) `shouldBe` "(lit-int 42)"
        it "negative int" $
            serialize (slit (LInt (-7))) `shouldBe` "(lit-int -7)"
        it "zero" $
            serialize (slit (LInt 0)) `shouldBe` "(lit-int 0)"
        it "float" $
            serialize (slit (LFloat 3.14)) `shouldBe` "(lit-float 3.14)"
        it "negative float" $
            serialize (slit (LFloat (-0.5))) `shouldBe` "(lit-float -0.5)"
        it "string" $
            serialize (slit (LString "hello")) `shouldBe` "(lit-str \"hello\")"
        it "empty string" $
            serialize (slit (LString "")) `shouldBe` "(lit-str \"\")"
        it "char" $
            serialize (slit (LChar 'a')) `shouldBe` "(lit-char 'a')"
        it "newline char" $
            serialize (slit (LChar '\n')) `shouldBe` "(lit-char '\n')"

    describe "serialize EApp" $ do
        it "f x" $
            serialize (sapp (sid "f") (sid "x")) `shouldBe` "(app (id f) (id x))"
        it "f 42" $
            serialize (sapp (sid "f") (slit (LInt 42))) `shouldBe` "(app (id f) (lit-int 42))"
        it "left-associative: f x y  →  (app (app f x) y)" $
            serialize (sapp (sapp (sid "f") (sid "x")) (sid "y"))
                `shouldBe` "(app (app (id f) (id x)) (id y))"
        it "nested: f (g x)" $
            serialize (sapp (sid "f") (sapp (sid "g") (sid "x")))
                `shouldBe` "(app (id f) (app (id g) (id x)))"

    describe "serialize ELam" $ do
        it "\\x.x" $
            serialize (slam "x" (sid "x")) `shouldBe` "(lam x (id x))"
        it "\\x.42" $
            serialize (slam "x" (slit (LInt 42))) `shouldBe` "(lam x (lit-int 42))"
        it "\\x.f x" $
            serialize (slam "x" (sapp (sid "f") (sid "x")))
                `shouldBe` "(lam x (app (id f) (id x)))"
        it "nested: \\x.\\y.x" $
            serialize (slam "x" (slam "y" (sid "x")))
                `shouldBe` "(lam x (lam y (id x)))"

    describe "serialize combined" $ do
        it "(\\x.x) 42" $
            serialize (sapp (slam "x" (sid "x")) (slit (LInt 42)))
                `shouldBe` "(app (lam x (id x)) (lit-int 42))"
        it "operator: (app (app + 1) 2)" $
            serialize (sapp (sapp (sid "+") (slit (LInt 1))) (slit (LInt 2)))
                `shouldBe` "(app (app (id +) (lit-int 1)) (lit-int 2))"
