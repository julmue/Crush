module Main where

import Prelude -- hiding (head,tail,fst,snd)
import Lambda
import Lambda.LambdaLib hiding (fst, snd, tail, head)

import Data.List

import Bound.Scope

main :: IO ()
main = undefined

bs :: [(String, NL String String)]
bs = [
      ("fun0", uname $ "x" ! Var "x" :@ Var "fun0" :@ Var "fun1")
    , ("fun1", uname $ Var "fun1" :@ Var "fun0")
    ]


-- 1. Step: abstract all names in the letrec itself
-- 1. Step: get all the binder names
names :: [String]
names = fmap fst bs

abstr :: Monad f => f String -> Scope Int f String
abstr = (abstract (`elemIndex` names))


