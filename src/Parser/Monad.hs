{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser.Monad where
import Lexer.Token
import Common.SourcePosition

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as M


data ParserState = ParserState
    { tokens        :: [Token]
    , infixOpTable  :: OpTable
    , prefixOpTable :: OpTable
    }

data ParserError = ParserError
    { parserErrToken   :: Token -- which token caused error
    , parserErrContext :: [String]
    } deriving (Eq)

instance Show ParserError where
  show :: ParserError -> String
  show e = "unexpected " ++ show tok ++ ", expected: " ++ (head stack)
        ++ concat (map ("\n\t in " ++) (tail stack))
        ++ "\nnear " ++ show (start . loc $ parserErrToken e)
           where stack = (parserErrContext e)
                 tok   = (parserErrToken e)

instance Semigroup ParserError where
  (<>) :: ParserError -> ParserError -> ParserError
  (<>) a b =
    let errPos = start . loc . parserErrToken
    in case compare (errPos a) (errPos b) of
          GT -> a
          LT -> b
          EQ -> a
    
instance Monoid ParserError where
    mempty :: ParserError
    mempty = ParserError (makeToken (SourceLocation (SourcePosition 0 0) (SourcePosition 0 0)) TEof) []

newtype Parser a = Parser
    { runParser :: StateT ParserState (Except ParserError) a }
    deriving (Functor, Applicative, Monad, MonadState ParserState, MonadError ParserError)

instance Alternative Parser where
    empty :: Parser a
    empty = throwParserError
    
    (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = do
        pre <- get
        catchError p $ \e -> do
          post <- get
          if tokens pre == tokens post
             then put pre >> catchError q (\e2 -> throwError (e <> e2))
             else throwError e

-- tryOrMessage :: Parser a -> String -> Parser a
-- p `tryOrMessage` m = catchError p (\e -> throwError e { parserErrMsg = Just m })

(<?>) :: Parser a -> String -> Parser a
-- p <?> expected = p `tryOrMessage` ("expected: " ++ expected)
p <?> expected = catchError p (\e -> throwError e { parserErrContext = expected : parserErrContext e })

throwParserError :: Parser a
throwParserError = do
    t <- peek
    throwError (ParserError t [])

peek :: Parser Token
peek = do
    st <- get
    case tokens st of
        (t:_) -> return t
        [] -> return (makeToken (SourceLocation (SourcePosition 0 0) (SourcePosition 0 0)) TEof)

next :: Parser Token
next = do
    st <- get
    case tokens st of
        (t:ts) -> put st { tokens = ts } >> return t
        []     -> throwParserError

satisfy :: (TokenKind -> Bool) -> Parser Token
satisfy p = do
    t <- peek
    if p (kind t)
        then next >> return t
        else throwParserError

token :: TokenKind -> Parser Token
token tk = satisfy (== tk)

match :: (TokenKind -> Maybe a) -> Parser a
match f = do
    t <- peek
    case f (kind t) of
        Nothing -> throwParserError
        Just a  -> next >> return a

data OpInfo = OpInfix  { lbp :: Int, rbp :: Int }
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
