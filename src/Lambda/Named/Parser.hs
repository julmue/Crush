module Lambda.Named.Parser
    (
      lambda
    , definition
    , definitions
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
import qualified Text.Parsec.Expr as E
import qualified Text.Parsec.String as S
import Text.Parsec.Language as L
import qualified Text.Parsec.Token as T

import Lambda.Named (Expr(..))

-- lexer
lexer :: T.TokenParser ()
lexer = T.makeTokenParser style
  where
    keys = ["letrec", "in"]
    ops = [".", "\\", ";","="]
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
variable :: S.Parser (Expr String)
variable = Var <$> identifier

atom :: S.Parser (Expr String)
atom = variable <|> parens expr

app :: S.Parser (Expr String)
app = atom `P.chainl1` (pure (:@))

lam :: S.Parser (Expr String)
lam = do
    reservedOp "\\"
    n <- identifier
    -- "." this is a hack because strange parsec behaviour with reservedOp "."
    P.char '.'
    P.spaces
    --
    e <- expr
    return (Lam n e)

letrec :: S.Parser (Expr String)
letrec = do
    reserved "letrec"
    ds <- defs
    reserved "in"
    term <- expr
    return (Letrec ds term)

def :: S.Parser (String, Expr String)
def = do
    n <- identifier
    reservedOp "="
    term <- expr
    return (n, term)

defs :: S.Parser [(String, Expr String)]
defs = braces (def `P.sepBy` semi)

expr :: S.Parser (Expr String)
expr = letrec <|> lam <|> app <|> atom

lambda :: String -> Either P.ParseError (Expr String)
lambda = P.parse expr "ExprParser"

definition :: String -> Either P.ParseError (String, Expr String)
definition = P.parse def "ExprParser"

definitions :: String -> Either P.ParseError [(String, Expr String)]
definitions = P.parse defs "ExprParser"

