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
    , iszro_
    , scc_
    , prd_
    , add_
    , sub_
    , mlt_
    , pow_
    , leqnat_
      -- numbers
    , zro_
    , one_
    , n2_
    , n3_
    , n4_
    , n5_
    , n6_
    , n7_
    , n8_
    , n9_
      -- equality
    , eqbool_
    , eqnat_
    ) where

import Language.Lambda.Syntax.Named.Exp

-- functions

-- S (substitution)
s_ :: Exp String
s_ = "x" ! "y" ! "z" ! Var "x" # Var "y" # (Var "x" # Var "z")

-- K (constant)
k_ :: Exp String
k_ = "x" ! "y" ! Var "x"

-- I (identity)
i_ :: Exp String
i_ = "x" ! Var "x"

-- omega
omega_ :: Exp String
omega_ = "x" ! Var"x" # Var"x"

_Omega_ :: Exp String
_Omega_ = omega_ # omega_

-- fixpoint
fix_ :: Exp String
fix_ = "g" ! ("x" ! Var"g" # (Var"x" # Var"x")) # ("x" ! Var"g" # (Var"x" # Var"x"))


-- logic

tru_ :: Exp String
tru_ = "t" ! "f" ! Var"t"

fls_ :: Exp String
fls_ = "t" ! "f" ! Var"f"

if_ :: Exp String
if_ = "p" ! "t" ! "f" ! Var"p" # Var"t" # Var"f"

not_ :: Exp String
not_ = "p" ! "x" ! "y" ! Var"p" # Var"y" # Var"x"

and_ :: Exp String
and_ = "p" ! "q" ! Var"p" # Var"q" # Var"p"

or_ :: Exp String
or_ = "p" ! "q" ! Var"p" # Var"p" # Var"q"

imp_ :: Exp String
imp_ = "p" ! "q" ! or_ # (not_ # Var"p") # Var"q"

iff_ :: Exp String
iff_ =  "p" ! "q" ! and_ # (imp_ # Var"p" # Var"q") # (imp_ # Var"q" # Var"p")

-- arithmetic

zro_ :: Exp String
zro_ = "f" ! "x" ! Var"x"

one_ :: Exp String
one_ = "f" ! "x" ! Var"f" # Var"x"

iszro_ :: Exp String
iszro_ = "n" ! Var"n" # ("_" ! fls_) # tru_

scc_ :: Exp String
scc_ = "n" ! "f" ! "x" ! Var"f" # (Var"n" # Var"f" # Var"x")

prd_ :: Exp String
prd_ = "n" ! "f" ! "x" !
    Var"n" # ("g" ! "h" ! Var"h" # (Var"g" # Var"f")) # ("_" ! Var"x") # i_

add_ :: Exp String
add_ = "x" ! "y" ! if_ # (iszro_ # Var"y") #
                   Var"x" #
                   (add_ # (scc_ # Var"x") # (prd_ # Var"y"))

sub_ :: Exp String
sub_ = "x" ! "y" ! if_ # (iszro_ # Var"y") #
                   Var"x" #
                   (sub_ # (prd_ # Var"x") # (prd_ # Var"y"))

mlt_ :: Exp String
mlt_ = "x" ! "y" ! if_ # (iszro_ # Var"y") #
                   zro_ #
                   (add_ # Var"x" # (mlt_ # Var"x" # (prd_ # Var"y")))

pow_ :: Exp String
pow_ = "b" ! "e" ! if_ # (iszro_ # Var"e") #
                   one_ #
                   (mlt_ # Var"b" # (pow_ # Var"b" # (prd_ # Var "e")))

--relation
leqnat_ :: Exp String
leqnat_ = "x" ! "y" ! iszro_ # (sub_ # Var"x" # Var"y")

-- equality
eqbool_ :: Exp String
eqbool_ = iff_

eqnat_ :: Exp String
eqnat_ = "x" ! "y" ! and_ # (leqnat_ # Var"x" # Var"y") # (leqnat_ # Var"y" # Var"x")


n2_ :: Exp String
n2_ = scc_ # one_

n3_ :: Exp String
n3_ = scc_ # n2_

n4_ :: Exp String
n4_ = scc_ # n3_

n5_ :: Exp String
n5_ = scc_ # n4_

n6_ :: Exp String
n6_ = scc_ # n5_

n7_ :: Exp String
n7_ = scc_ # n6_

n8_ :: Exp String
n8_ = scc_ # n7_

n9_ :: Exp String
n9_ = scc_ # n8_
