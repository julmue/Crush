module Language.Lambda.Syntax.Named.Parser
    (
      expression
    , exprP
    ) where

{-
=== Grammar

<expr> = <variable>
       | <expr> <expr>
       | \<var>.<expr>
       | let <expr> in <expr>
       | (<expr>)

Todo: constants (Integers, Chars, Strings, ...)
-}

import Control.Applicative

import qualified Text.Parsec as P
import qualified Text.Parsec.String as S
import Text.Parsec.Language as L
import qualified Text.Parsec.Token as T

import Language.Lambda.Syntax.Named.Exp

lexer :: T.TokenParser ()
lexer = T.makeTokenParser style
  where
    keys = ["let", "in"]
    ops = ["\\", "λ", ".", ";", "="]
    style = emptyDef
        { T.reservedNames = keys
        , T.reservedOpNames = ops
        , T.identStart = P.alphaNum <|> P.char '_'
        , T.identLetter = P.alphaNum <|> P.char '_'
        , T.commentLine = "--"
        , T.commentStart = "{-"
        , T.commentEnd  = "-}"
        }

parens :: S.Parser a -> S.Parser a
parens = T.parens lexer

dot :: S.Parser String
dot = T.dot lexer

reserved :: String -> S.Parser ()
reserved = T.reserved lexer

reservedOp :: String -> S.Parser ()
reservedOp = T.reservedOp lexer

identifier :: S.Parser String
identifier = T.identifier lexer

whiteSpace :: S.Parser ()
whiteSpace = T.whiteSpace lexer

variableP :: S.Parser (Exp String)
variableP = Var <$> identifier

atomP :: S.Parser (Exp String)
atomP = variableP <|> parens exprP

appP :: S.Parser (Exp String)
appP = atomP `P.chainl1` pure App

lamP :: S.Parser (Exp String)
lamP = do
    reservedOp "\\" <|> reservedOp "λ"
    n <- identifier
    _ <- dot
    e <- exprP
    return (Lam n e)

letP :: S.Parser (Exp String)
letP = do
    reserved "let"
    d <- defP
    reserved "in"
    term <- exprP
    return (Let d term)

defP :: S.Parser (String, Exp String)
defP = do
    n <- identifier
    reservedOp "="
    term <- exprP
    return (n, term)

exprP :: S.Parser (Exp String)
exprP = do
    whiteSpace
    letP <|> lamP <|> appP <|> atomP

expression :: String -> Either P.ParseError (Exp String)
expression = P.parse exprP "ExpParser"
