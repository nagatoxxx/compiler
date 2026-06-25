module Parser.Ast where

data Expr
    = EApp Expr Expr
    | ELam [String] Expr
    | EIdent String
    | ELit Literal
    deriving Show

data Literal
    = LInt Int
    | LFloat Double
    | LString String
    | LChar Char
    deriving Show
