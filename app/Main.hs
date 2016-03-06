module Main where

import Lambda
import LambdaLib

main :: IO ()
main = undefined

no :: LambdaTerm t => t -> t
no = normalOrder
cbn :: LambdaTerm t => t -> t
cbn = callByName
cbv :: LambdaTerm t => t -> t
cbv = callByValue
