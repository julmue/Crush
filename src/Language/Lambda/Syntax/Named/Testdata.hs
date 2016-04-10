module Language.Lambda.Syntax.Named.Testdata
    ( -- function
      s
    , k
    , i
    , omega
    , _Omega
    , y
      -- logic
    , tru
    , fls
    , if_
    , not_
    , and_
    , or_
      -- arithmetic
    , zro
    , iszro
    , scc
    , prd
    , pls
    , sub
    , mlt
    , pow
    , leqnat
      -- equality
    , eqbool
    , eqnat
    ) where

import Language.Lambda.Syntax.Named.Exp

-- functions

-- S (substitution)
s = "x" ! "y" ! "z" ! Var "x" # Var "y" # (Var "x" # Var "z")

-- K (constant)
k = "x" ! "y" ! Var "x"

-- I (identity)
i = "x" ! Var "x"

-- omega
omega = "x" ! Var"x" # Var"x"

_Omega = omega # omega

-- fixpoint
y = "g" ! ("x" ! Var"g" # (Var"x" # Var"x")) # ("x" ! Var"g" # (Var"x" # Var"x"))


-- logic

tru = "t" ! "f" ! Var"t"

fls = "t" ! "f" ! Var"f"

if_ = "p" ! "t" ! "f" ! Var"p" # Var"t" # Var"f"

not_ = "p" ! "x" ! "y" ! Var"p" # Var"y" # Var"x"

and_ = "p" ! "q" ! Var"p" # Var"q" # Var"p"

or_ = "p" ! "q" ! Var"p" # Var"p" # Var"q"

imp = "p" ! "q" ! or_ # (not_ # Var"p") # Var"q"

iff =  "q" ! "q" ! and_ # (imp # Var"p" # Var"q") # (imp # Var"q" # Var"p")

-- arithmetic

zro = "f" ! "x" ! Var"x"

iszro = "n" ! Var"n" # ("_" ! Var"fls") # tru

scc = "n" ! "f" ! "x" ! Var"f" # (Var"n" # Var"f" # Var"x")

prd = "n" ! "f" ! "x" !
    Var"n" # ("g" ! "h" ! Var"h" # (Var"g" # Var"f")) # ("_" ! Var"x") # i

pls = "x" ! "y" ! Var"x" # (scc # Var"y")

sub = "x" ! "y" ! Var"y" # (prd # Var"x")

mlt = "x" ! "y" ! "f" ! Var"x" # (Var"y" # Var"f")

pow = "b" ! "e" ! Var"e" # Var"b"

--relation

leqnat = "x" ! "y" ! iszro # (sub # Var"x" # Var"y")

-- equality

eqbool = iff

eqnat = "x" ! "y" ! and_ # (leqnat # Var"x" # Var"y") # (leqnat # Var"y" # Var"x")
