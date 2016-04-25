module Language.Lambda.Syntax.Named.Parser
    (
      expression
    , definition
    , definitions
    , expr
    , def
    , defs
    ) where

{-
=== Grammar

<expr> = <variable>
       | <expr> <expr>
       | \<var>.<expr>
       | letrec { <var> = <expr> ;
                  ...
                  <var> = <expr> }
          in <expr>
       | (<expr>)

Todo: constants (Integers, Chars, Strings, ...)

-}

import Control.Applicative

import qualified Text.Parsec as P
import qualified Text.Parsec.String as S
import Text.Parsec.Language as L
import qualified Text.Parsec.Token as T

import Language.Lambda.Syntax.Named.Exp (Exp(Var,App,Lam,Letrec))

-- lexer
lexer :: T.TokenParser ()
lexer = T.makeTokenParser style
  where
    keys = ["letrec", "in"]
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

semi :: S.Parser String
semi = T.semi lexer

braces :: S.Parser a -> S.Parser a
braces = T.braces lexer

parens :: S.Parser a -> S.Parser a
parens = T.parens lexer

reserved :: String -> S.Parser ()
reserved = T.reserved lexer

reservedOp :: String -> S.Parser ()
reservedOp = T.reservedOp lexer

identifier :: S.Parser String
identifier = T.identifier lexer

-- parser
variable :: S.Parser (Exp String)
variable = Var <$> identifier

atom :: S.Parser (Exp String)
atom = variable <|> parens expr

app :: S.Parser (Exp String)
app = atom `P.chainl1` (pure App)

lam :: S.Parser (Exp String)
lam = do
    (reservedOp "\\" <|> reservedOp "λ")
    n <- identifier
    -- "." this is a hack because strange parsec behaviour with reservedOp "."
    _ <- P.char '.'
    P.spaces
    --
    e <- expr
    return (Lam n e)

letrec :: S.Parser (Exp String)
letrec = do
    reserved "letrec"
    ds <- defs
    reserved "in"
    term <- expr
    return (Letrec ds term)

def :: S.Parser (String, Exp String)
def = do
    n <- identifier
    reservedOp "="
    term <- expr
    return (n, term)

defs :: S.Parser [(String, Exp String)]
defs = braces (def `P.sepBy` semi)

expr :: S.Parser (Exp String)
expr = letrec <|> lam <|> app <|> atom

expression :: String -> Either P.ParseError (Exp String)
expression = P.parse expr "ExpParser"

definition :: String -> Either P.ParseError (String, Exp String)
definition = P.parse def "ExpParser"

definitions :: String -> Either P.ParseError [(String, Exp String)]
definitions = P.parse defs "ExpParser"

