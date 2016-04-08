module Main where

import Prelude hiding (head,tail,fst,snd,not,and,or)

import Bound.Scope
import qualified Bound.Unwrap as BU


import Lambda as L
import Lambda.Named.Parser as PL

main :: IO ()
main = putStrLn . show $ "hello"

infixr 6 |>

(|>) :: Eq a => a -> NL a a -> NL a a
(|>) = lam
