{-# LANGUAGE FlexibleInstances #-}

module Lexer.Monad where

import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
import Data.Char as C

data LexerPosition = LexerPosition 
    { line :: Int
    , col  :: Int
    } deriving (Eq)

instance Show LexerPosition where
  show p = "line: " ++ show (line p) ++ ", col: " ++ show (col p)

data LexerState = LexerState
    { source :: String
    , pos    :: LexerPosition
    } deriving (Show)

data LexerError = LexerError { message :: String
                             , tpos    :: LexerPosition
                             }
                  deriving (Eq)

instance Show LexerError where
  show e = message e ++ " at " ++ show (tpos e)

instance Monoid LexerError where
    mempty = LexerError "" (LexerPosition 1 1)

instance Semigroup LexerError where
    LexerError a p <> LexerError _ _ = LexerError a p

type Lexer a = StateT LexerState (Except LexerError) a

-- errors
errEndOfInput :: LexerPosition -> LexerError 
errEndOfInput = LexerError "unexpected end of input"

errUnexpectedChar :: LexerPosition -> LexerError
errUnexpectedChar = LexerError "unexpected character"

lexError :: (LexerPosition -> LexerError) -> Lexer a
lexError e = do
  st <- get
  throwError (e (pos st)) 

-- pos
updatePos :: Char -> LexerState -> LexerState
updatePos c st = st
    { source = tail (source st)
    , pos = newPos
    }
    where isNewline = (c == '\n')
          oldPos    = pos st
          newPos = LexerPosition 
                { line = (if isNewline then (+1) else id) . line $ oldPos
                , col  = (if isNewline then 1 else col oldPos + 1)
                }

-- прочитать следующий символ
peek :: Lexer (Maybe Char)
peek = do
    st <- get
    case (source st) of
        []    -> return Nothing
        (x:_) -> return (Just x)

-- потребить символ и перейти к следующему
next :: Lexer ()
next = do
    mc <- peek
    case mc of
        Just c  -> modify (updatePos c)
        Nothing -> lexError errEndOfInput

-- потребить символ, удовлетворяющий p, и перейти к следующему
satisfy :: (Char -> Bool) -> Lexer Char
satisfy p = do
    mc <- peek
    case mc of
        Nothing -> lexError errEndOfInput
        Just c  -> if p c
            then next >> return c
            else lexError errUnexpectedChar

char :: Char -> Lexer Char
char c = satisfy (== c)

any :: Lexer Char
any = satisfy (\_ -> True)

anyOf :: String -> Lexer Char
anyOf cs = satisfy (`elem` cs)

noneOf :: String -> Lexer Char
noneOf cs = satisfy (`notElem` cs)

skipSpaces :: Lexer ()
skipSpaces = many (satisfy C.isSpace) *> return ()
