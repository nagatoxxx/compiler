{-# LANGUAGE CPP #-}

module Lexer.Token where
import Common.SourcePosition

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

type Token = WithSourceLocation TokenKind

kind :: Token -> TokenKind
kind t = value t

makeToken :: SourceLocation -> TokenKind -> Token
makeToken = WithSourceLocation

instance Show Token where
    show :: Token -> String
#ifdef DEBUG
    show t = (show . kind $ t) ++ " (" ++ (show . loc $ t) ++ ")"
#else
    show t = (show . kind $ t)
#endif

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
