module Main where

import Prelude hiding (head,tail,fst,snd,not,and,or)
import Lambda
import Lambda.LambdaLib -- hiding (fst, snd, tail, head)

import Bound.Scope
import qualified Bound.Unwrap as BU

main :: IO ()
main = putStrLn . show . normalOrder $ test1

infixr 6 |>

(|>) :: Eq a => a -> NL a a -> NL a a
(|>) = lam

test1 = let_
  [ -- functions
    ("const", "x" |> "y" |> V"x")
  , ("fix", "g" |> ("x" |> V"g" :$ ( V"x" :$ V"x")) :$ ("x" |> V"g" :$ ( V"x" :$ V"x")))
    -- logic
  , ("tru", "t" |> "f" |> V "t")
  , ("fls", "t" |> "f" |> V "f")
  , ("not", "p" |> "t" |> "f" |> V"p" :$ V"f" :$ V"t") -- flips arguments !Attention ... depends on the evaluation order!
  , ("and", "p" |> "q" |> V"p" :$ V"q" :$ V"p")
  , ("or" , "p" |> "q" |> V"p" :$ V"p" :$ V"q")
  , ("imp", "p" |> "q" |> (V"not" :$ V"p") :$ V"or" :$ V"q")
  , ("iff", "p" |> "q" |> (V"p" :$ V"imp" :$ V"q") :$ V"and" :$ (V"q" :$ V"imp" :$ V"p"))
  , ("if" , "p" |> "t" |> "f" |> V"p" :$ V"t" :$ V"f")
    -- arithmetic
  , ("zro",    "f" |> "x" |> V"x")
  , ("scc",    "n" |> "f" |> "x" |> V"f" :$ (V"n" :$ V"f" :$ V"x"))
  , ("prd",    "n" |> "f" |> "x" |> V"n" :$ ("g" |> "h" |> V"h" :$ (V"g" :$ V"f")) :$ ("u"|> V"x") :$ ("x"|>V"x"))
  , ("one",    V"scc" :$ V"zro")
  , ("two",    V"scc" :$ V"one")
  , ("three",  V"scc" :$ V"two")
  , ("add",   "x" |> "y" |> V"x" :$ V"scc" :$ V"y")
  , ("mul",   "x" |> "y" |> V"x" :$ (V"add" :$ V"y") :$ V"zro")
  , ("iszro",  "n" |> V"n" :$ (V"const" :$ V"fls") :$ V"tru")
  -- we have a lambda calculus extended with recursive bindings, so we don't need to use fix
--  , ("fac",   "x" |> V"x" :$ V"one" :$ ("n" |> V"mul" :$ V"x" :$ (V"fac" :$ V"n")))
  , ("fac" ,  "x" |> V"if" :$ (V"iszro" :$ V"x") :$ V"one" :$ (V"mul" :$ V"x" :$ (V"fac" :$ (V"prd" :$ V"x"))))
  , ("eqnat", "x" |> "y" |>
            V"if" :$ (V"and" :$ (V"iszro" :$ V"x") :$ (V"iszro" :$ V"y")) :$
                V"tru" :$
                (V"if" :$ (V"or" :$ (V"iszro" :$ V"x") :$ (V"iszro" :$ V"y")) :$
                   V"fls" :$
                   (V"eqnat" :$ (V"prd" :$ V"x") :$ (V"prd" :$ V"y"))))
  , ("sumto", "x" |> V"x" :$ V"zro" :$ ("n" |> V"add" :$ V"x" :$ (V"sumto" :$ V"n"))) -- sum [1 .. x]
  --
  , ("n5",     V"add" :$ V"two" :$ V"three")
  , ("n6",     V"add" :$ V"three" :$ V"three")
  , ("n17",    V"add" :$ V"n6" :$ (V"add" :$ V"n6" :$ V"n5"))
  , ("n37",    V"scc" :$ (V"mul" :$ V"n6" :$ V"n6"))
  , ("n703",   V"sumto" :$ V"n37")
  , ("n720",   V"fac" :$ V"n6")
  ] (V"eqnat" :$ V"n720" :$ (V"add" :$ V"n703" :$ V"n17"))

