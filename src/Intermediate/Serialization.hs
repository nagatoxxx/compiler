module Intermediate.Serialization where

import Parser.Ast
import Common.SourcePosition (WithSourceLocation(WithSourceLocation))

serialize :: Expr -> String
serialize (WithSourceLocation _ e)
  = case e of
      EApp f x -> "(app " ++ serialize f ++ " " ++ serialize x ++ ")"
      ELam (WithSourceLocation _ v) b -> "(lam " ++ v ++ " " ++ serialize b ++ ")"
      EIdent (WithSourceLocation _ n) -> "(id " ++ n ++ ")"
      ELit (WithSourceLocation _ l)   -> case l of
        LInt i    -> "(lit-int " ++ show i ++ ")"
        LFloat f  -> "(lit-float " ++ show f ++ ")"
        LString s -> "(lit-str \"" ++ s ++ "\")"
        LChar c   -> "(lit-char '" ++ [c] ++ "')"
  
