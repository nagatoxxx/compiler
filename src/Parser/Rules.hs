module Parser.Rules where

import Control.Applicative
import Control.Monad (guard)
import Control.Monad.State
import Control.Monad.Except

import Lexer.Token as T
import Parser.Monad as P
import Parser.Ast

import Common.SourcePosition

-- TODO use (@@)
withLocation :: Parser a -> Parser (WithSourceLocation a)
withLocation p = do
    (WithSourceLocation start _) <- gets (head . tokens)
    e                            <- p
    m                            <- gets lastToken
    let end = maybe (mempty) (loc) m
    return $ WithSourceLocation (start <> end) e

-- Literal
pInt :: Parser LiteralF
pInt = LInt <$> match T.asInt

pFloat :: Parser LiteralF
pFloat = LFloat <$> match T.asFloat

pString :: Parser LiteralF
pString = LString <$> match T.asString

pChar :: Parser LiteralF
pChar = LChar <$> match T.asChar

pLiteral :: Parser Literal
pLiteral = (withLocation $ pInt <|> pFloat <|> pChar <|> pString) <?> "literal"

-- Ident as String
pIdent :: Parser Ident
pIdent = withLocation (match T.asIdent) <?> "identifier"

pAtom :: Parser Expr
pAtom = withLocation
      $ (ELit <$> pLiteral)
    <|> (value <$> (token TLParen *> pExpr <* token TRParen))
    <|> EIdent <$> pIdent
    <?> "atom"

nud :: Parser Expr
nud = pAtom
  <|> pLam
  <|> (do
    op   <- withLocation (match T.asOp)  -- Ident = WithSourceLocation String
    let opExpr = EIdent op @@ loc op     -- Expr
    t    <- gets infixOpTable
    maybe (throwError mempty)
        (\p -> do
            body <- pPratt p
            return $ EApp opExpr body @@ (loc opExpr <> loc body))
        (lbp <$> opInfo (value op) t))

_pPratt :: Expr -> Int -> Parser Expr
_pPratt l minBp = do
  t <- gets infixOpTable
  do
    op <- withLocation (match T.asOp)  -- Ident
    let opExpr = EIdent op @@ loc op   -- Expr
    info <- maybe (throwError mempty) return (opInfo (value op) t)
    case info of
        OpInfix lb rb -> do
            guard (lb > minBp)
            r <- pPratt rb
            let inner = EApp opExpr l @@ (loc l <> loc op)
            let outer = EApp inner r  @@ (loc l <> loc r)
            _pPratt outer minBp
        OpPrefix _ -> throwError mempty
  <|>
  do
    r <- pAtom
    _pPratt (EApp l r @@ (loc l <> loc r)) minBp
  <|> return l

pPratt :: Int -> Parser Expr
pPratt minBp = do
  l <- nud
  _pPratt l minBp

pExpr :: Parser Expr
pExpr = pPratt 0 <?> "expr"

pLam :: Parser Expr
pLam = do
    bs     <- satisfy (== TBackslash)
    params <- some pIdent
    _ <- token TDot
    body   <- pExpr
    return $ foldr
        (\p e -> ELam p e @@ (loc bs <> loc e))
        body
        params

parse :: [Token] -> Either ParserError Expr
parse ts = runExcept
         $ fst
       <$> runStateT (runParser (pExpr <* (token TEof <?> "end of input"))) initialState
    where initialState = ParserState { tokens        = ts
                                     , lastToken     = Nothing
                                     , infixOpTable  = defaultInfixOpTable
                                     , prefixOpTable = defaultPrefixOpTable
                                     }
