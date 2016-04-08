module Main where

import Control.Monad.Except
import System.IO
import Prelude hiding (head,tail,fst,snd,not,and,or)

import Bound.Scope
import qualified Bound.Unwrap as BU

import Lambda as L
import Lambda.Named.Parser as PL

import qualified Text.Parsec as P

main :: IO ()
main = putStrLn . show $ "hello"

data LambdaError =
      Parser P.ParseError

showError :: LambdaError -> String
showError (Parser err) = show err


testFile = readFile "./examples/cookedSingleExpr.lam"
