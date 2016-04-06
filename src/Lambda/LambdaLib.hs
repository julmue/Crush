module Lambda.LambdaLib
    (
    -- ski combinators
      s
    , k
    , i
    -- booleans
    , tru
    , fls
    , test
    -- arithmetic
    , scc
    , c0
    , plus
    , times
    , iszro
    -- pairs
    , pair
    , pFst
    , pSnd
    -- y-combinator (least fixpoint)
    , fix
    ) where

import Lambda.Named

-- s k i combinators
s = "f" ! "g" ! "x" ! Var "f" :@ Var "x" :@ (Var "g" :@ Var "x")
k = "x" ! "y" ! Var "x"
i = "x" ! Var "x"

-- booleans
tru = "t" ! "f" ! Var "t"

fls = "t" ! "f" ! Var "f"

test = "l" ! "m" ! "n" ! Var "l" :@ Var "m" :@ Var "n"

-- arithmetic / Church encoding
scc = "n" ! "s" ! "z" ! Var "s" :@ (Var "n" :@ Var "s" :@ Var "z")

c0 = "s" ! "z" ! Var "z"

plus = "m" ! "n" ! "s" ! "z" !
    Var "m" :@ Var "s" :@ (Var "n" :@ Var "s" :@ Var "z")

times = "m" ! "n" ! Var "m" :@ (plus :@ Var "n") :@ c0

iszro = "m" ! Var "m" :@ ("x" ! fls) :@ tru

-- pairs
pair = "f" ! "s" ! "b" ! Var "b" :@ Var "f" :@ Var "s"

pFst = "p" ! Var "p" :@ tru

pSnd = "p" ! Var "p" :@ fls

-- fixpoint combinator
fix = "f" !
    ("x" ! Var "f" :@ ("y" ! Var "x" :@ Var "x" :@ Var "y")) :@
    ("x" ! Var "f" :@ ("y" ! Var "x" :@ Var "x" :@ Var "y"))
