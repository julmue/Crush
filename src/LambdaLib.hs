module LambdaLib where

import Lambda

-- combinators

s = Lam "f" $ Lam "g" $ Lam "x" $ (Var "f" :@ Var "x") :@ (Var "g" :@ Var "x")
k = Lam "x" $ Lam "y" $ Var "x"
i = Lam "x" $ Var "x"

tru = Lam "t" $ Lam "f" $ Var "t"
fls = Lam "t" $ Lam "f" $ Var "f"
test = Lam "bool" $ Lam "branch1" $ Lam "branch2" $ Var "bool" :@ Var "branch1" :@ Var "branch2"

pair = Lam "f" $ Lam "s" $ Lam "b" $ Var "b" :@ Var "f" :@ Var "s"
fst = Lam "p" $ Var "p" :@ tru
snd = Lam "p" $ Var "p" :@ fls

-- church numerals
scc = Lam "n" $ Lam "s" $ Lam "z" $ Var "s" :@ (Var "n" :@ Var "s" :@ Var "z")
c0 = Lam "s" $ Lam "z" $ Var "z"
c1 = scc :@ c0
c2 = scc :@ c1

plus = Lam "n" $ Lam "n" $ Lam "s" $ Var "m" :@ Var "s" :@ (Var "n" :@ Var "s" :@ Var "z")
times = Lam "m" $ Lam "n" $ Var "m" :@ (plus :@ Var "n") :@ c0
