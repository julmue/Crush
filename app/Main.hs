module Main where

import Prelude -- hiding (head,tail,fst,snd)
import Lambda
import Lambda.LambdaLib hiding (fst, snd, tail, head)

import Data.List

import Bound.Scope

main :: IO ()
main = putStrLn . show . normalOrder $ test1

infixr 6 |>

(|>) :: Eq a => a -> NL a a -> NL a a
(|>) = lam

cooked :: NL String String
-- -- cooked = fromJust $ closed $ let_
cooked = let_
  [ ("False",  "f" |> "t" |> V"f")
  , ("True",   "f" |> "t" |> V"t")
  , ("if",     "b" |> "t" |> "f" |> V"b" :$ V"f" :$ V"t")
  , ("Zero",   "z" |> "s" |> V"z")
  , ("Succ",   "n" |> "z" |> "s" |> V"s" :$ V"n")
  , ("one",    V"Succ" :$ V"Zero")
  , ("two",    V"Succ" :$ V"one")
  , ("three",  V"Succ" :$ V"two")
  , ("isZero", "n" |> V"n" :$ V"True" :$ ("m" |> V"False"))
  , ("const",  "x" |> "y" |> V"x")
  , ("Pair",   "a" |> "b" |> "p" |> V"p" :$ V"a" :$ V"b")
  , ("fst",    "ab" |> V"ab" :$ ("a" |> "b" |> V"a"))
  , ("snd",    "ab" |> V"ab" :$ ("a" |> "b" |> V"b"))
  -- we have a lambda calculus extended with recursive bindings, so we don't need to use fix
  , ("add",    "x" |> "y" |> V"x" :$ V"y" :$ ("n" |> V"Succ" :$ (V"add" :$ V"n" :$ V"y")))
  , ("mul",    "x" |> "y" |> V"x" :$ V"Zero" :$ ("n" |> V"add" :$ V"y" :$ (V"mul" :$ V"n" :$ V"y")))
  , ("fac",    "x" |> V"x" :$ V"one" :$ ("n" |> V"mul" :$ V"x" :$ (V"fac" :$ V"n")))
  , ("eqnat",  "x" |> "y" |> V"x" :$ (V"y" :$ V"True" :$ (V"const" :$ V"False")) :$ ("x1" |> V"y" :$ V"False" :$ ("y1" |> V"eqnat" :$ V"x1" :$ V"y1")))
  , ("sumto",  "x" |> V"x" :$ V"Zero" :$ ("n" |> V"add" :$ V"x" :$ (V"sumto" :$ V"n")))
  --
  , ("n5",     V"add" :$ V"two" :$ V"three")
  , ("n6",     V"add" :$ V"three" :$ V"three")
  , ("n17",    V"add" :$ V"n6" :$ (V"add" :$ V"n6" :$ V"n5"))
  , ("n37",    V"Succ" :$ (V"mul" :$ V"n6" :$ V"n6"))
  , ("n703",   V"sumto" :$ V"n37")
  , ("n720",   V"fac" :$ V"n6")
  ] (V"eqnat" :$ V"n720" :$ (V"add" :$ V"n703" :$ V"n17"))

test1 = let_
  [ ("False",  "f" |> "t" |> V"f")
  , ("True",   "f" |> "t" |> V"t")
  , ("if",     "b" |> "t" |> "f" |> V"b" :$ V"f" :$ V"t")
  , ("Zero",   "z" |> "s" |> V"z")
  , ("Succ",   "n" |> "z" |> "s" |> V"s" :$ V"n")
  , ("one",    V"Succ" :$ V"Zero")
  , ("two",    V"Succ" :$ V"one")
  , ("three",  V"Succ" :$ V"two")
  , ("isZero", "n" |> V"n" :$ V"True" :$ ("m" |> V"False"))
  , ("const",  "x" |> "y" |> V"x")
  , ("Pair",   "a" |> "b" |> "p" |> V"p" :$ V"a" :$ V"b")
  , ("fst",    "ab" |> V"ab" :$ ("a" |> "b" |> V"a"))
  , ("snd",    "ab" |> V"ab" :$ ("a" |> "b" |> V"b"))
  -- we have a lambda calculus extended with recursive bindings, so we don't need to use fix
  , ("add",    "x" |> "y" |> V"x" :$ V"y" :$ ("n" |> V"Succ" :$ (V"add" :$ V"n" :$ V"y")))
  , ("mul",    "x" |> "y" |> V"x" :$ V"Zero" :$ ("n" |> V"add" :$ V"y" :$ (V"mul" :$ V"n" :$ V"y")))
  , ("fac",    "x" |> V"x" :$ V"one" :$ ("n" |> V"mul" :$ V"x" :$ (V"fac" :$ V"n")))
  , ("eqnat",  "x" |> "y" |> V"x" :$ (V"y" :$ V"True" :$ (V"const" :$ V"False")) :$ ("x1" |> V"y" :$ V"False" :$ ("y1" |> V"eqnat" :$ V"x1" :$ V"y1")))
  , ("sumto",  "x" |> V"x" :$ V"Zero" :$ ("n" |> V"add" :$ V"x" :$ (V"sumto" :$ V"n")))
  --
  , ("n5",     V"add" :$ V"two" :$ V"three")
  , ("n6",     V"add" :$ V"three" :$ V"three")
  , ("n17",    V"add" :$ V"n6" :$ (V"add" :$ V"n6" :$ V"n5"))
  , ("n37",    V"Succ" :$ (V"mul" :$ V"n6" :$ V"n6"))
  , ("n703",   V"sumto" :$ V"n37")
  , ("n720",   V"fac" :$ V"n6")
  ] (V"fac" :$ V"three")

simple = let_
  [ ("tru", "t" |> "f" |> V "t")
  , ("fls", "f" |> "f" |> V "f")
  , ("if",  "bool" |> "t" |> "f" |> V "bool" :$ V "t" :$ V "f")
  ] (V "if" :$ V "tru" :$ V "it's true" :$ V "it's false")
