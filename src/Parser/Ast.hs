module Parser.Ast where

data Expr
    = EApp Expr Expr
    | ELam String Expr
    | EAtom Atom
    deriving Show

data Atom
    = ALit Literal
    | AIdent String
    | APExpr Expr -- '(' expr ')'
    deriving Show

data Literal
    = LInt Int
    | LFloat Double
    | LString String
    | LChar Char
    deriving Show
