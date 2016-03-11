{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module NamelessTerm where

import Control.Monad (ap)
import Prelude.Extras
import Bound

infixl 9 :@

data Exp a
    = V a
    | Exp a :@ Exp a
    | Lam (Scope () Exp a)
    deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

instance Eq1 Exp
instance Ord1 Exp
instance Show1 Exp
instance Read1 Exp

instance Monad Exp where
    return = V
    (V a) >>= f = f a
    (fun :@ arg) >>= f = (fun >>= f) :@ (arg >>= f)
    (Lam e) >>= f = Lam (e >>>= f)

instance Applicative Exp where
    pure = V
    (<*>) = ap

-- smart constructor
lam :: Eq a => a -> Exp a -> Exp a
lam v b = Lam (abstract1 v b)

infix 0 !
(!) :: Eq a => a -> Exp a -> Exp a
(!) = lam

nf :: Exp a -> Exp a
nf e@V{} = e
nf e@(Lam b) = Lam . toScope . nf . fromScope $ b
nf e@(f :@ a) = case whnf f of
    Lam b -> nf (instantiate1 a b)
    f' -> nf f' :@ nf a

whnf :: Exp a -> Exp a
whnf e@V{} = e
whnf e@Lam{} = e
whnf (f :@ a) = case whnf f of
    Lam b -> whnf (instantiate1 a b)
    f' -> f' :@ a

