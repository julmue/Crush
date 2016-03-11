{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Lambda.Nameless where

import Control.Monad (ap)
import Prelude.Extras
import Bound

infixl 9 :$

data Exp a
    = V a
    | Exp a :$ Exp a
    | L (Scope () Exp a)
    deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

instance Eq1 Exp
instance Ord1 Exp
instance Show1 Exp
instance Read1 Exp

instance Monad Exp where
    return = V
    (V a) >>= f = f a
    (fun :$ arg) >>= f = (fun >>= f) :$ (arg >>= f)
    (L e) >>= f = L (e >>>= f)

instance Applicative Exp where
    pure = V
    (<*>) = ap


