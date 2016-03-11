{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Lambda.Nameless where

import Control.Monad (ap)
import Prelude.Extras
import Bound

infixl 9 :$

data NLL a
    = V a
    | NLL a :$ NLL a
    | L (Scope () NLL a)
    deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

instance Eq1 NLL
instance Ord1 NLL
instance Show1 NLL
instance Read1 NLL

instance Monad NLL where
    return = V
    (V a) >>= f = f a
    (fun :$ arg) >>= f = (fun >>= f) :$ (arg >>= f)
    (L e) >>= f = L (e >>>= f)

instance Applicative NLL where
    pure = V
    (<*>) = ap

-- smart constructor
lam :: Eq a => a -> NLL a -> NLL a
lam v b = L (abstract1 v b)

infix 0 !
(!) :: Eq a => a -> NLL a -> NLL a
(!) = lam
