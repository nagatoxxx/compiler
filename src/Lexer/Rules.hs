module Lexer.Rules where

import Lexer.Monad
import Lexer.Token

import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Char as C

lexIdent :: Lexer Token
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

lexInt :: Lexer Token
lexInt = do
    sign   <- optional (char '-')
    digits <- some digit
    let num = maybe "" (:[]) sign ++ digits
    return (TInt (read num))

lexFloat :: Lexer Token
lexFloat = do
    sign    <- optional (char '-')
    digits1 <- some digit
    _       <- char '.'
    digits2 <- some digit
    let num = maybe "" (:[]) sign ++ digits1 ++ "." ++ digits2
    return (TFloat (read num))

lexString :: Lexer Token
lexString = TString <$> (char '"' *> many (noneOf "\"") <* char '"')

escapeChar :: Char -> Char
escapeChar 'n'  = '\n'
escapeChar 't'  = '\t'
escapeChar '\\' = '\\'
escapeChar '\'' = '\''
escapeChar '0'  = '\0'
escapeChar c    = c

lexChar :: Lexer Token
lexChar = TChar <$> (char '\'' *> body <* char '\'')
  where body = char '\\' *> (escapeChar <$> anyOf "nt\\'0")
           <|> noneOf "\\'"

lexPunct :: Lexer Token
lexPunct =
    (char '(' *> return TLParen)   <|>
    (char ')' *> return TRParen)
    
opChars :: String
opChars = "+-*/->\\"

lexOp :: Lexer Token
lexOp = do
    op <- some (anyOf opChars)
    case op of
        "->" -> return TArrow
        "\\" -> return TBackslash
        _    -> return (TOp op)

lexToken :: Lexer Token
lexToken = lexIdent
       <|> lexFloat
       <|> lexInt
       <|> lexString
       <|> lexChar
       <|> lexOp
       <|> lexPunct

lexTokens :: Lexer [Token]
lexTokens = do
    skipSpaces
    mc <- peek
    case mc of
        Nothing -> return []
        Just _  -> do
            t  <- lexToken
            ts <- lexTokens
            return (t : ts)

tokenize :: String -> Either LexerError [Token]
tokenize s = runExcept
           $ (++ [TEof]) <$>
           fst <$> runStateT lexTokens initialState
         where initialState = LexerState { source = s
                                         , pos    = LexerPosition { line = 1
                                                                  , col = 1
                                                                  }
                                         }
