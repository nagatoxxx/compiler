{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser.Monad where

import Lexer.Token

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as M

data ParserState = ParserState
    { tokens        :: [Token]
    , infixOpTable  :: OpTable
    , prefixOpTable :: OpTable
    }

-- TODO: ParserErrorUnexpected, ParserError... (cannot found op in table)
data ParserError = ParserError
    { parserErrPos      :: SourcePosition
    , parserErrGot      :: TokenKind
    , parserErrExpected :: String
    } deriving (Eq)

instance Show ParserError where
  show :: ParserError -> String
  show e
    | null (parserErrExpected e) =
        "unexpected token: got " ++ show (parserErrGot e)
        ++ " near " ++ show (parserErrPos e)
    | otherwise =
        "unexpected token: expected " ++ parserErrExpected e
        ++ ", got " ++ show (parserErrGot e)
        ++ " near " ++ show (parserErrPos e)

instance Semigroup ParserError where
    (<>) :: ParserError -> ParserError -> ParserError
    a <> _ = a
    
instance Monoid ParserError where
    mempty :: ParserError
    mempty = ParserError (SourcePosition 0 0) TEof ""

newtype Parser a = Parser
    { runParser :: StateT ParserState (Except ParserError) a }
    deriving (Functor, Applicative, Monad, MonadState ParserState, MonadError ParserError)

instance Alternative Parser where
    empty :: Parser a
    empty = throwParserError
    (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = do
        st <- get
        catchError p $ \_ -> put st >> q

-- TODO: push to grammar stack
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

next :: Parser Token
next = do
    st <- get
    case tokens st of
        (t:ts) -> put st { tokens = ts } >> return t
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

data OpAssoc = OpLeft | OpRight

data OpInfo = OpInfix  { rbp :: Int, lbp :: Int }
            | OpPrefix { bp :: Int}
            
type OpTable = M.Map String OpInfo

defaultInfixOpTable :: OpTable
defaultInfixOpTable = M.fromList
  [ ("+", OpInfix 5 5)
  , ("-", OpInfix 5 5)
  , ("*", OpInfix 6 6)
  , ("/", OpInfix 6 6)
  ]

defaultPrefixOpTable :: OpTable
defaultPrefixOpTable = M.fromList
  [ ("-", OpPrefix 10)
  , ("+", OpPrefix 10)
  ]

opInfo :: String -> OpTable -> Maybe OpInfo
opInfo s t = M.lookup s t
