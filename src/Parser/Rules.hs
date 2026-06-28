module Parser.Rules where

import Control.Applicative
import Control.Monad (guard)
import Control.Monad.State
import Control.Monad.Except

import Lexer.Token as T
import Parser.Monad as P
import Parser.Ast
import Common.SourcePosition

withLocation :: Parser ExprF -> Parser Expr
withLocation p = do
  (WithSourceLocation start _) <- gets (head . tokens)
  e                            <- p
  (WithSourceLocation end _)   <- gets (head . tokens)
  return $ makeExpr (start <> end) e

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
pAtom = withLocation
      $ (ELit <$> pLiteral)
    <|> (EIdent <$> pIdent)
    <|> (value <$> (token TLParen *> pExpr <* token TRParen))
    <?> "atom"

nud :: Parser Expr
nud = pAtom
  <|> pLam
  <|> (do
          op <- withLocation $ EIdent <$> match T.asOp
          t  <- gets infixOpTable
          case value op of
            EIdent opName -> maybe (throwError mempty)
                (\p -> do
                    body <- pPratt p -- Expr
                    return (EApp <$> op <*> body))
                (lbp <$> (opInfo (opName)) t)
            _unreachable -> throwParserError)

_pPratt :: Expr -> Int -> Parser Expr
_pPratt l minBp = do
  t <- gets infixOpTable
  do
    op <- withLocation $ EIdent <$> match T.asOp
    case value op of
        EIdent opName -> do
            info <- maybe (throwError mempty) return (opInfo opName t)
            case info of
                OpInfix lb rb -> do
                    guard (lb > minBp)
                    right <- pPratt rb
                    _pPratt (EApp <$> (EApp <$> op <*> l) <*> right) minBp
                OpPrefix _ -> throwError mempty
        _unreachable -> throwError mempty
  <|>
  do
    right <- pAtom
    _pPratt (EApp <$> l <*> right) minBp
  <|> return l

pPratt :: Int -> Parser Expr
pPratt minBp = do
  l <- nud
  _pPratt l minBp

pExpr :: Parser Expr
pExpr = pPratt 0 <?> "expr"

pLam :: Parser Expr
pLam = do
    bs   <- satisfy (== TBackslash)
    params <- some pIdent
    _ <- token TArrow
    body <- pExpr
    return $ makeExpr (loc bs <> loc body) (ELam params (value body))

parse :: [Token] -> Either ParserError Expr
parse ts = runExcept
         $ fst
       <$> runStateT (runParser (pExpr <* (token TEof <?> "end of input"))) initialState
    where initialState = ParserState { tokens        = ts
                                     , infixOpTable  = defaultInfixOpTable
                                     , prefixOpTable = defaultPrefixOpTable
                                     }
