module Parser.Ast where

import Common.SourcePosition

data ExprF
    = EApp Expr Expr   
    | ELam Ident Expr -- (abstraction var), body
    | EIdent Ident
    | ELit Literal
    deriving (Eq)

surround :: Char -> Char -> String -> String
surround b e s = b : s ++ [e]

instance Show ExprF where
    show :: ExprF -> String
    show (EIdent i) = "EIdent " ++ (surround '(' ')' $ show (value i))
    show (ELam p e) = "ELam " ++ (surround '(' ')' $ show (value p) ++ " " ++ show e)
    show (EApp f x) = "EApp " ++ (surround '(' ')' $ show f ++ " " ++ show x)
    show (ELit l)   = "ELit " ++ (surround '(' ')' $ show (value l))

data LiteralF
    = LInt Int
    | LFloat Double
    | LString String
    | LChar Char
    deriving (Show, Eq)

type Expr    = WithSourceLocation ExprF
type Ident   = WithSourceLocation String
type Literal = WithSourceLocation LiteralF
