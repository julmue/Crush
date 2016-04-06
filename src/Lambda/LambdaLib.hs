module Lambda.LambdaLib
    (
      s
    , k
    , i
    , tru
    , fls
    , test
    , scc
    , c0
    , plus
    , times
    ) where

import Lambda.Named

s = "f" ! "g" ! "x" ! Var "f" :@ Var "x" :@ (Var "g" :@ Var "x")
k = "x" ! "y" ! Var "x"
i = "x" ! Var "x"

tru = "t" ! "f" ! Var "t"

fls = "t" ! "f" ! Var "f"

test = "l" ! "m" ! "n" ! Var "l" :@ Var "m" :@ Var "n"

scc = "n" ! "s" ! "z" ! Var "s" :@ (Var "n" :@ Var "s" :@ Var "z")

c0 = "s" ! "z" ! Var "z"

plus = "m" ! "n" ! "s" ! "z" ! Var "m" :@ Var "s" :@ (Var "n" :@ Var "s" :@ Var "z")

times = "m" ! "n" ! Var "m" :@ (plus :@ Var "n") :@ c0
