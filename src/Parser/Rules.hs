module Parser.Rules where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Except

import Lexer.Token as T
import Parser.Monad as P
import Parser.Ast

-- Literal
pInt :: Parser Literal
pInt = LInt <$> match T.asInt

pFloat :: Parser Literal
pFloat = LFloat <$> match T.asFloat

pString :: Parser Literal
pString = LString <$> match T.asString

pChar :: Parser Literal
pChar = LChar <$> match T.asChar

pLiteral :: Parser Literal
pLiteral = (pInt <|> pFloat <|> pChar <|> pString) <?> "literal"

-- Ident as String
pIdent :: Parser String
pIdent = match T.asIdent <?> "identifier"

pAtom :: Parser Atom
pAtom = ((ALit <$> pLiteral)
    <|> (AIdent <$> pIdent)
    <|> (APExpr <$> (token TLParen *> pExpr <* token TRParen))) <?> "atom"

pApp :: Parser Expr
pApp = do
  f <- pAtom
  args <- some pAtom
  return $ foldl EApp (EAtom f) (map EAtom args)

pExpr :: Parser Expr
pExpr = (pApp <|> pLam <|> EAtom <$> pAtom) <?> "expr"

pLam :: Parser Expr
pLam = (ELam <$> (token TBackslash *> pIdent) <*> (token TArrow *> pExpr)) <?> "lambda"

parse :: [Token] -> Either ParserError Expr
parse ts = runExcept
        $ fst
     <$> runStateT (runParser (pExpr <* (token TEof <?> "end of input"))) initialState
    where initialState = ParserState { tokens = ts }
