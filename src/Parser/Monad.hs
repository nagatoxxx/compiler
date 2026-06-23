{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser.Monad where

import Lexer.Token

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative

data ParserState = ParserState
    { tokens :: [Token]
    }

data ParserError = ParserError
    { parserErrPos      :: SourcePosition
    , parserErrGot      :: TokenKind
    , parserErrExpected :: String
    } deriving (Eq)

instance Show ParserError where
  show e
    | null (parserErrExpected e) =
        "unexpected token: got " ++ show (parserErrGot e)
        ++ " near " ++ show (parserErrPos e)
    | otherwise =
        "unexpected token: expected " ++ parserErrExpected e
        ++ ", got " ++ show (parserErrGot e)
        ++ " near " ++ show (parserErrPos e)

instance Semigroup ParserError where
    a <> _ = a

instance Monoid ParserError where
    mempty = ParserError (SourcePosition 0 0) TEof ""

newtype Parser a = Parser
    { runParser :: StateT ParserState (Except ParserError) a }
    deriving (Functor, Applicative, Monad, MonadState ParserState, MonadError ParserError)

instance Alternative Parser where
    empty = throwParserError
    p <|> q = do
        st <- get
        catchError p $ \_ -> put st >> q

(<?>) :: Parser a -> String -> Parser a
p <?> expected = catchError p (\e -> throwError e { parserErrExpected = expected })

throwParserError :: Parser a
throwParserError = do
    t <- peek
    throwError (ParserError (tokenPos t) (tokenKind t) "")

peek :: Parser Token
peek = do
    st <- get
    case tokens st of
        (t:_) -> return t
        []    -> return (Token TEof (SourcePosition 0 0))

next :: Parser ()
next = do
    st <- get
    case tokens st of
        (_:ts) -> put st { tokens = ts }
        []     -> throwParserError

satisfy :: (TokenKind -> Bool) -> Parser Token
satisfy p = do
    t <- peek
    if p (tokenKind t)
        then next >> return t
        else throwError (ParserError (tokenPos t) (tokenKind t) "")

token :: TokenKind -> Parser Token
token tk = satisfy (== tk)

match :: (TokenKind -> Maybe a) -> Parser a
match f = do
    t <- peek
    case f (tokenKind t) of
        Nothing -> throwError (ParserError (tokenPos t) (tokenKind t) "")
        Just a  -> next >> return a
