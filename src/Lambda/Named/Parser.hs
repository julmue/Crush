module Lambda.Named.Parser
    (
      parser
    ) where

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
    style = emptyDef {
        T.reservedOpNames = [".","\\",";"],
        T.commentLine = "#"
    }

semi :: S.Parser String
semi = T.semi lexer

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

lambda :: S.Parser (Lambda String)
lambda = do
    reservedOp "\\"
    n <- identifier
    -- "." this is a hack because strange parsec behaviour with reservedOp "."
    P.char '.'
    P.spaces
    e <- expr
    return (Lam n e)

app :: S.Parser (Lambda String)
app = atom `P.chainl1` (pure (:@))

expr :: S.Parser (Lambda String)
expr = lambda <|> app <|> atom

parser s = P.parse expr "LambdaParser"

-- parser :: S.Parser [Lambda String]
-- parser = expr `P.sepBy` semi
