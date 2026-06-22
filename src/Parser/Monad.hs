{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- для deriving Functor, State...

module Parser.Monad where

import Lexer.Token

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative

data ParserState = ParserState
    { tokens :: [Token]
    , pos    :: Int
    }

data ParserError = ParserError String
    deriving (Show, Eq)

instance Monoid ParserError where
    mempty = ParserError "" 

instance Semigroup ParserError where
    ParserError a <> ParserError _ = ParserError a

newtype Parser a = Parser
    { runParser :: StateT ParserState (Except ParserError) a } 
    deriving (Functor, Applicative, Monad, MonadState ParserState, MonadError ParserError)

instance Alternative Parser where
    empty = throwError errUnexpectedToken
    p <|> q = do
        st <- get
        catchError p $ \_ ->
            put st >> q

-- errors
errUnexpectedToken :: ParserError
errUnexpectedToken = ParserError "unexpected token"

-- parserError

peek :: Parser (Maybe Token)
peek = do
    st <- get
    return (case (tokens st) of
        (t:_) -> Just t
        []    -> Nothing)

next :: Parser ()
next = do
    st <- get
    case (tokens st) of
        (_:ts) -> put st { tokens = ts, pos = pos st + 1 }
        []     -> throwError errUnexpectedToken

satisfy :: (Token -> Bool) -> Parser Token
satisfy p = do
    mt <- peek
    case mt of
        Just t  -> case p t of
            True  -> next >> return t
            False -> throwError errUnexpectedToken

        Nothing -> throwError errUnexpectedToken

token :: Token -> Parser Token
token t = satisfy (== t)

match :: (Token -> Maybe a) -> Parser a
match f = do
    mt <- peek
    case mt of
        Nothing -> throwError errUnexpectedToken
        Just t  -> case f t of
            Nothing -> throwError errUnexpectedToken
            Just a  -> next >> return a
