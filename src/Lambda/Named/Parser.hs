module Lambda.Named.Parser
    (
      lambda
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

import Lambda.Named (Lambda(..))

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
variable :: S.Parser (Lambda String)
variable = Var <$> identifier

atom :: S.Parser (Lambda String)
atom = variable <|> parens expr

app :: S.Parser (Lambda String)
app = atom `P.chainl1` (pure (:@))

lam :: S.Parser (Lambda String)
lam = do
    reservedOp "\\"
    n <- identifier
    -- "." this is a hack because strange parsec behaviour with reservedOp "."
    P.char '.'
    P.spaces
    --
    e <- expr
    return (Lam n e)

letrec :: S.Parser (Lambda String)
letrec = do
    reserved "letrec"
    defs <- definitions
    reserved "in"
    term <- expr
    return (Letrec defs term)

definition :: S.Parser (String, Lambda String)
definition = do
    n <- identifier
    reservedOp "="
    term <- expr
    return (n, term)

definitions :: S.Parser [(String, Lambda String)]
definitions = braces (definition `P.sepBy` semi)

expr :: S.Parser (Lambda String)
expr = letrec <|> lam <|> app <|> atom

lambda :: String -> Either P.ParseError (Lambda String)
lambda = P.parse expr "LambdaParser"

