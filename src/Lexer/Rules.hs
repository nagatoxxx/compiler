module Lexer.Rules where

import Lexer.Monad
import Lexer.Token

import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Char as C

lexIdent :: Lexer TokenKind
lexIdent = TIdent <$> ident

digit :: Lexer Char
digit = satisfy C.isDigit

alpha :: Lexer Char
alpha = satisfy C.isAlpha

ident :: Lexer String
ident = do
    c  <- (char '_' <|> alpha)
    cs <- many (alpha <|> digit)
    return (c : cs)

lexInt :: Lexer TokenKind
lexInt = do
    sign   <- optional (char '-')
    digits <- some digit
    let num = maybe "" (:[]) sign ++ digits
    return (TInt (read num))

lexFloat :: Lexer TokenKind
lexFloat = do
    sign    <- optional (char '-')
    digits1 <- some digit
    _       <- char '.'
    digits2 <- some digit
    let num = maybe "" (:[]) sign ++ digits1 ++ "." ++ digits2
    return (TFloat (read num))

lexString :: Lexer TokenKind
lexString = TString <$> (char '"' *> many (noneOf "\"") <* char '"')

escapeChar :: Char -> Char
escapeChar 'n'  = '\n'
escapeChar 't'  = '\t'
escapeChar '\\' = '\\'
escapeChar '\'' = '\''
escapeChar '0'  = '\0'
escapeChar c    = c

lexChar :: Lexer TokenKind
lexChar = TChar <$> (char '\'' *> body <* char '\'')
  where body = char '\\' *> (escapeChar <$> anyOf "nt\\'0")
           <|> noneOf "\\'"

lexPunct :: Lexer TokenKind
lexPunct =
    (char '(' *> return TLParen)   <|>
    (char ')' *> return TRParen)

opChars :: String
opChars = "+-*/->\\"

lexOp :: Lexer TokenKind
lexOp = do
    op <- some (anyOf opChars)
    case op of
        "->" -> return TArrow
        "\\" -> return TBackslash
        _    -> return (TOp op)

lexTokenKind :: Lexer TokenKind
lexTokenKind = lexIdent
           <|> lexFloat
           <|> lexInt
           <|> lexString
           <|> lexChar
           <|> lexOp
           <|> lexPunct

lexToken :: Lexer Token
lexToken = do
    p  <- gets pos
    tk <- lexTokenKind
    return (Token tk p)

lexTokens :: Lexer [Token]
lexTokens = do
    skipSpaces
    mc <- peek
    case mc of
        Nothing -> do
            p <- gets pos
            return [Token TEof p]
        Just _  -> do
            t  <- lexToken
            ts <- lexTokens
            return (t : ts)

tokenize :: String -> Either LexerError [Token]
tokenize s = runExcept
           $ fst <$> runStateT lexTokens initialState
         where initialState = LexerState { source = s
                                         , pos    = SourcePosition { line = 1
                                                                   , col  = 1
                                                                   }
                                         }
