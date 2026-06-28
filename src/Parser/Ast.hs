module Parser.Ast where

import Common.SourcePosition

data ExprF
    = EApp ExprF ExprF
    -- TODO: переделать 
    | ELam [String] ExprF
    | EIdent String
    | ELit Literal
    deriving (Show, Eq)

data Literal
    = LInt Int
    | LFloat Double
    | LString String
    | LChar Char
    deriving (Show, Eq)

type Expr = WithSourceLocation ExprF

instance Show Expr where
  show :: Expr -> String
  show (WithSourceLocation l e) = show e ++ " @ " ++ show l

makeExpr :: SourceLocation -> ExprF -> Expr
makeExpr = WithSourceLocation 
