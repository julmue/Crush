{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module Language.Lambda.Syntax.Nameless.Exp (
      Alpha (Alpha)
    , runAlpha
    , Exp (Var, App, Lam, Let)
--    , bound
--    , free
--    , fold
--    , mapExp
--    , mapAlpha
    , (#)
    , (!)
    , lam_
    , gLam_
    , let_
    , gLet_
) where

import Control.Monad (ap)
import Data.List (elemIndex)

import Bound
import Prelude.Extras

-- -----------------------------------------------------------------------------
-- nameless representation

-- the name of the binder that got abstracted;
-- this information is not relevant for alpha-equivalence

data Alpha n = Alpha { runAlpha :: n }
    deriving (Show, Read, Functor, Foldable, Traversable)

instance Eq n => Eq (Alpha n) where
    _ == _ = True


data Exp n a =
      Var !a
    -- fun arg
    | App (Exp n a) (Exp n a)
    -- alpha scope
    | Lam (Alpha n) (Scope () (Exp n) a)
    -- alphas defs scope
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

-- fold :: forall n b f .
--        (forall a . a -> f a)
--     -> (forall a . f a -> f a -> f a)
--     -> (forall a . Alpha n -> Scope () f a -> f a)
--     -> (forall a . Alpha [n] -> [Scope Int f a] -> (Scope Int f a) -> f a)
--     -> (Exp n) b -> f b
-- fold v _ _ _ (Var n) = v n
-- fold v a l lrc (fun_ `App` arg_) = a (fold v a l lrc fun_) (fold v a l lrc arg_)
-- fold v a l lrc (Lam alpha_ scope_) = l alpha_ hScope
--   where hScope = (hoistScope (fold v a l lrc ) scope_)
-- fold v a l lrc (Letrec defs exp) = lrc hDefs hExp
--   where
--     hDefs = hoistScope (fold v a l lrc) <$> defs
--     hExp = hoistScope (fold v a l lrc) exp

-- mapAlpha :: (n -> m) -> (Exp n) a -> (Exp m) a
-- mapAlpha f = fold Var App l Letrec
--   where
--     l a s = Lam (f <$> a) s
--
-- mapExp :: (n -> m) -> (a -> b) -> Exp n a -> Exp m b
-- mapExp f g e = mapAlpha f . fmap g $ e

-- | a smart constructor for abstractions
infixl 9 #

(#) :: (Exp n a) -> (Exp n a) -> (Exp n a)
(#) = App

infixr 6 !

(!) :: Eq a => a -> Exp a a -> Exp a a
(!) = lam_

lam_ :: Eq a => a -> Exp a a -> Exp a a
lam_ a e = gLam_ a id e

gLam_ :: Eq a => a -> (a -> n) -> Exp n a -> Exp n a
gLam_ a f e = Lam (Alpha (f a)) (abstract1 a e)

-- | a smart constructor for let bindings
let_ :: Eq a => (a, Exp a a) -> Exp a a -> Exp a a
let_ a e = gLet_ a id e

gLet_ :: Eq a => (a, Exp n a) -> (a -> n) -> Exp n a -> Exp n a
gLet_ (a,d) f e = Let (Alpha (f a)) d (abstract1 a e)

