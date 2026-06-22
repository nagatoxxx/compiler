module Lexer.Token where

data Token
    = TInt    Int
    | TFloat  Double
    | TString String
    | TChar   Char
    | TIdent  String
    | TOp     String
    | TLParen  | TRParen
    | TLBracket | TRBracket
    | TBackslash
    | TArrow
    | TEof
    deriving (Show, Eq)

asIdent :: Token -> Maybe String
asIdent (TIdent s) = Just s
asIdent _          = Nothing

asInt :: Token -> Maybe Int
asInt (TInt n) = Just n
asInt _        = Nothing

asFloat :: Token -> Maybe Double
asFloat (TFloat n) = Just n
asFloat _          = Nothing

asString :: Token -> Maybe String
asString (TString s) = Just s
asString _           = Nothing

asChar :: Token -> Maybe Char
asChar (TChar c) = Just c
asChar _         = Nothing
