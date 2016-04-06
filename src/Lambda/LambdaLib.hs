{-# LANGUAGE NoImplicitPrelude #-}

module Lambda.LambdaLib
    (
    -- ski combinators
      s
    , k
    , i
    , o       -- should be 'omega'
    , omega   -- should be 'Omega'
    -- booleans
    , tru
    , fls
    , ifthenelse
    , and
    , or
    , not
    -- arithmetic
    , scc
    , c0
    , plus
    , times
    , pow
    , iszro
    -- pairs
    , pair
    , fst
    , snd
    -- lists
    , cons
    , head
    , tail
    -- y-combinator (least fixpoint)
    , fix
    ) where

import Lambda.Named

-- s k i combinators
s = "f" ! "g" ! "x" ! Var "f" :@ Var "x" :@ (Var "g" :@ Var "x")

k = "x" ! "y" ! Var "x"

i = "x" ! Var "x"

o = "x" ! Var "x" :@ Var "x"

omega = o :@ o


-- logic and predicates
tru = "t" ! "f" ! Var "t"

fls = "t" ! "f" ! Var "f"

ifthenelse = "l" ! "m" ! "n" ! Var "l" :@ Var "m" :@ Var "n"

and = "p" ! "q" ! Var "p" :@ Var "q" :@ Var "p"

or = "p" ! "q" ! Var "p" :@ Var "p" :@ Var "q"

not = "p" ! "a" ! "b" ! Var "p" :@ Var "b" :@ Var "a"

-- arithmetic / Church encoding
scc = "n" ! "s" ! "z" ! Var "s" :@ (Var "n" :@ Var "s" :@ Var "z")

c0 = "s" ! "z" ! Var "z"

plus = "m" ! "n" ! "s" ! "z" !
    Var "m" :@ Var "s" :@ (Var "n" :@ Var "s" :@ Var "z")

times = "m" ! "n" ! Var "m" :@ (plus :@ Var "n") :@ c0

pow = "base" ! "exp" ! Var "e" :@ Var "b"

iszro = "m" ! Var "m" :@ ("x" ! fls) :@ tru

-- pairs
pair = "f" ! "s" ! "b" ! Var "b" :@ Var "f" :@ Var "s"

fst = "p" ! Var "p" :@ tru

snd = "p" ! Var "p" :@ fls

-- lists
cons = "a" ! "b" ! "f" ! Var "f" :@ Var "a" :@ Var "b"

head = "c" ! Var "c" :@ ("a" ! "b" ! Var "a")

tail = "c" ! Var "c" :@ ("a" ! "b" ! Var "b")


-- fixpoint combinator
fix = "f" !
    ("x" ! Var "f" :@ ("y" ! Var "x" :@ Var "x" :@ Var "y")) :@
    ("x" ! Var "f" :@ ("y" ! Var "x" :@ Var "x" :@ Var "y"))
