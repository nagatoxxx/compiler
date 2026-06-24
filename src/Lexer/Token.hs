module Lexer.Token where

data SourcePosition = SourcePosition
    { line :: Int
    , col  :: Int
    } deriving (Eq)

instance Show SourcePosition where
    show p = "line: " ++ show (line p) ++ ", col: " ++ show (col p)

data TokenKind
    = TInt    Int
    | TFloat  Double
    | TString String
    | TChar   Char
    | TIdent  String
    | TOp     String
    | TLParen
    | TRParen
    | TLBracket
    | TRBracket
    | TBackslash
    | TArrow
    | TEof
    deriving (Show, Eq)

data Token = Token
    { tokenKind :: TokenKind , tokenPos  :: SourcePosition
    } deriving (Eq)

instance Show Token where
    show = show . tokenKind

asIdent :: TokenKind -> Maybe String
asIdent (TIdent s) = Just s
asIdent _          = Nothing

asInt :: TokenKind -> Maybe Int
asInt (TInt n) = Just n
asInt _        = Nothing

asFloat :: TokenKind -> Maybe Double
asFloat (TFloat n) = Just n
asFloat _          = Nothing

asString :: TokenKind -> Maybe String
asString (TString s) = Just s
asString _           = Nothing

asChar :: TokenKind -> Maybe Char
asChar (TChar c) = Just c
asChar _         = Nothing

asOp :: TokenKind -> Maybe String
asOp (TOp s) = Just s
asOp _       = Nothing
