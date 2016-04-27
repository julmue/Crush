{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

module Language.Lambda.Syntax.Nameless.Exp (
      Alpha (Alpha)
    , runAlpha
    , Exp (Var, App, Lam, Let)
--    , bound
--    , free
    , fold
    , mapExp
    , mapAlpha
    , (#)
    , (!)
    , lam_
    , gLam_
    , let_
    , gLet_
) where

import Control.Monad (ap)

import Bound
import Bound.Scope
import Prelude.Extras

data Alpha n = Alpha { runAlpha :: n }
    deriving (Show, Read, Functor, Foldable, Traversable)

instance Eq n => Eq (Alpha n) where
    _ == _ = True

data Exp n a =
      Var !a
    | App (Exp n a) (Exp n a)
    | Lam (Alpha n) (Scope () (Exp n) a)
    | Let (Alpha n) (Exp n a) (Scope () (Exp n) a)
    deriving (Eq,Show,Read,Functor,Foldable,Traversable)

instance (Eq n) => Eq1 (Exp n)
instance (Show n) => Show1 (Exp n)
instance (Read n) => Read1 (Exp n)

instance Monad (Exp n) where
    return = Var
--  (>>=) :: Exp n a -> (a -> Exp n b) -> Exp n B
    (Var a) >>= g = g a
    (f `App` a) >>= g = (f >>= g) `App` (a >>= g)
    (Lam n s) >>= g = Lam n (s >>>= g)
    (Let ns d e) >>= g = Let ns (d >>= g) (e >>>= g)

instance Applicative (Exp n) where
    pure = Var
    (<*>) = ap

fold :: forall n b f .
       (forall a . a -> f a)
    -> (forall a . f a -> f a -> f a)
    -> (forall a . Alpha n -> Scope () f a -> f a)
    -> (forall a . Alpha n -> Exp n a -> Scope () f a -> f a)
    -> (Exp n) b -> f b
fold v _ _ _ (Var n) = v n
fold v a l lt (fun `App` arg) = a (fold v a l lt fun) (fold v a l lt arg)
fold v a l lt (Lam alpha scope) = l alpha scope'
  where
    scope' = (hoistScope (fold v a l lt) scope)
fold v a l lt (Let alpha def scope) = lt alpha def scope'
  where
    scope' = hoistScope (fold v a l lt) scope

mapAlpha :: (n -> m) -> (Exp n) a -> (Exp m) a
mapAlpha f = fold Var App l lt
  where
    l a s = Lam (f <$> a) s
    lt a e s = Let (f <$> a) (mapAlpha f e) s

mapExp :: (n -> m) -> (a -> b) -> Exp n a -> Exp m b
mapExp f g e = mapAlpha f . fmap g $ e

-- | a smart constructor for abstractions
infixl 9 #

(#) :: Exp n a -> Exp n a -> Exp n a
(#) = App

infixr 6 !

(!) :: Eq a => a -> Exp a a -> Exp a a
(!) = lam_

lam_ :: Eq a => a -> Exp a a -> Exp a a
lam_ a = gLam_ a id

gLam_ :: Eq a => a -> (a -> n) -> Exp n a -> Exp n a
gLam_ a f e = Lam (Alpha (f a)) (abstract1 a e)

-- | a smart constructor for let bindings
let_ :: Eq a => (a, Exp a a) -> Exp a a -> Exp a a
let_ a = gLet_ a id

gLet_ :: Eq a => (a, Exp n a) -> (a -> n) -> Exp n a -> Exp n a
gLet_ (a,d) f e = Let (Alpha (f a)) d (abstract1 a e)
