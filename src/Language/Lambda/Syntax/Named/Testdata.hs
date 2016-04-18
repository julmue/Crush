module Language.Lambda.Syntax.Named.Testdata
    ( -- function
      s_
    , k_
    , i_
    , omega_
    , _Omega_
    , fix_
      -- logic
    , tru_
    , fls_
    , if_
    , not_
    , and_
    , or_
    , imp_
    , iff_
      -- arithmetic
    , zro_
    , one_
    , iszro_
    , scc_
    , prd_
    , add_
    , sub_
    , mlt_
    , pow_
    , leqnat_
      -- equality
    , eqbool_
    , eqnat_
    ) where

import Language.Lambda.Syntax.Named.Exp

-- functions

-- S (substitution)
s_ = "x" ! "y" ! "z" ! Var "x" # Var "y" # (Var "x" # Var "z")

-- K (constant)
k_ = "x" ! "y" ! Var "x"

-- I (identity)
i_ = "x" ! Var "x"

-- omega
omega_ = "x" ! Var"x" # Var"x"

_Omega_ = omega_ # omega_

-- fixpoint
fix_ = "g" ! ("x" ! Var"g" # (Var"x" # Var"x")) # ("x" ! Var"g" # (Var"x" # Var"x"))


-- logic

tru_ = "t" ! "f" ! Var"t"

fls_ = "t" ! "f" ! Var"f"

if_ = "p" ! "t" ! "f" ! Var"p" # Var"t" # Var"f"

not_ = "p" ! "x" ! "y" ! Var"p" # Var"y" # Var"x"

and_ = "p" ! "q" ! Var"p" # Var"q" # Var"p"

or_ = "p" ! "q" ! Var"p" # Var"p" # Var"q"

imp_ = "p" ! "q" ! or_ # (not_ # Var"p") # Var"q"

iff_ =  "p" ! "q" ! and_ # (imp_ # Var"p" # Var"q") # (imp_ # Var"q" # Var"p")

-- arithmetic

zro_ = "f" ! "x" ! Var"x"

one_ = "f" ! "x" ! Var"f" # Var"x"

iszro_ = "n" ! Var"n" # ("_" ! fls_) # tru_

scc_ = "n" ! "f" ! "x" ! Var"f" # (Var"n" # Var"f" # Var"x")

prd_ = "n" ! "f" ! "x" !
    Var"n" # ("g" ! "h" ! Var"h" # (Var"g" # Var"f")) # ("_" ! Var"x") # i_

--pls_ = "x" ! "y" ! Var"x" # (scc_ # Var"y")
add_ = "x" ! "y" ! if_ # (iszro_ # Var"y") #
                   Var"x" #
                   (add_ # (scc_ # Var"x") # (prd_ # Var"y"))

sub_ = "x" ! "y" ! if_ # (iszro_ # Var"y") #
                   Var"x" #
                   (sub_ # (prd_ # Var"x") # (prd_ # Var"y"))

-- mlt_ = "x" ! "y" ! "f" ! Var"x" # (Var"y" # Var"f")
mlt_ = "x" ! "y" ! if_ # (iszro_ # Var"y") #
                   zro_ #
                   (add_ # Var"x" # (mlt_ # Var"x" # (prd_ # Var"y")))

--pow_ = "b" ! "e" ! Var"e" # Var"b"
pow_ = "b" ! "e" ! if_ # (iszro_ # Var"e") #
                   one_ #
                   (mlt_ # Var"b" # (pow_ # Var"b" # (prd_ # Var "e")))

--relation

leqnat_ = "x" ! "y" ! iszro_ # (sub_ # Var"x" # Var"y")

-- equality

eqbool_ = iff_

eqnat_ = "x" ! "y" ! and_ # (leqnat_ # Var"x" # Var"y") # (leqnat_ # Var"y" # Var"x")
