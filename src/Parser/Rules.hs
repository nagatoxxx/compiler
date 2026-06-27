module Parser.Rules where

import Control.Applicative
import Control.Monad (guard)
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
pLiteral = pInt <|> pFloat <|> pChar <|> pString <?> "literal"

-- Ident as String
pIdent :: Parser String
pIdent = match T.asIdent <?> "identifier"

pAtom :: Parser Expr
pAtom = (ELit <$> pLiteral)
    <|> (EIdent <$> pIdent)
    <|> (token TLParen *> pExpr <* token TRParen)
    <?> "atom"

nud :: Parser Expr
nud = pAtom
  <|> pLam
  <|> (do
          op <- match T.asOp
          t  <- gets infixOpTable
          maybe (throwError mempty) (\p -> EApp (EIdent op) <$> pPratt p) (lbp <$> (opInfo op t)))

_pPratt :: Expr -> Int -> Parser Expr
_pPratt l minBp = do
  t <- gets infixOpTable
  do
    op  <- match T.asOp
    info <- maybe (throwError mempty) return (opInfo op t)
    case info of
      OpInfix lb rb -> do
        guard (lb > minBp)
        right <- pPratt rb
        _pPratt (EApp (EApp (EIdent op) l) right) minBp
      OpPrefix _ -> throwError mempty 
  <|>
  do
    right <- pAtom
    _pPratt (EApp l right) minBp
  <|> return l

pPratt :: Int -> Parser Expr
pPratt minBp = do
  l <- nud
  _pPratt l minBp

pExpr :: Parser Expr
pExpr = pPratt 0 <?> "expr"

pLam :: Parser Expr
pLam = (ELam <$> (token TBackslash *> some pIdent) <*> (token TArrow *> pExpr)) <?> "lambda expression"

parse :: [Token] -> Either ParserError Expr
parse ts = runExcept
         $ fst
       <$> runStateT (runParser (pExpr <* (token TEof <?> "end of input"))) initialState
    where initialState = ParserState { tokens        = ts
                                     , infixOpTable  = defaultInfixOpTable
                                     , prefixOpTable = defaultPrefixOpTable
                                     }
