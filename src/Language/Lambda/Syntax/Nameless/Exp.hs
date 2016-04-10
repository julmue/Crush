{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

module Language.Lambda.Syntax.Nameless.Exp (
      Alpha (Alpha)
    , runAlpha
    , Exp (Var, App, Lam, Letrec)
--    , bound
--    , free
    , fold
    , mapExp
    , mapAlpha
    , (#)
    , (!)
    , lam
    , gLam
    , letrec
) where

import Control.Monad (ap)
import Data.List (elemIndex)

import Bound
import Bound.Scope
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
      Var a
    | App (Exp n a) (Exp n a)
    | Lam (Alpha n) (Scope () (Exp n) a)
    | Letrec (Alpha [n]) [Scope Int (Exp n) a] (Scope Int (Exp n) a)
    deriving (Eq,Show,Read,Functor,Foldable,Traversable)


instance (Eq n) => Eq1 (Exp n)
instance (Show n) => Show1 (Exp n)
instance (Read n) => Read1 (Exp n)

instance Monad (Exp n) where
    return = Var
--  (>>=) :: Exp n a -> (a -> Exp n b) -> Exp n B
    (Var a) >>= f = f a
    (fun `App` arg) >>= f = (fun >>= f) `App` (arg >>= f)
    (Lam n scope) >>= f = Lam n (scope >>>= f)
    (Letrec n bs scope) >>= f = Letrec n (map (>>>= f) bs) (scope >>>= f)

instance Applicative (Exp n) where
    pure = Var
    (<*>) = ap

fold :: forall n b f .
       (forall a . a -> f a)
    -> (forall a . f a -> f a -> f a)
    -> (forall a . Alpha n -> Scope () f a -> f a)
    -> (forall a . Alpha [n] -> [Scope Int f a] -> (Scope Int f a) -> f a)
    -> (Exp n) b -> f b
fold v _ _ _ (Var n) = v n
fold v a l lrc (fun `App` arg) = a (fold v a l lrc fun) (fold v a l lrc arg)
fold v a l lrc (Lam alpha scope) = l alpha hScope
  where hScope = (hoistScope (fold v a l lrc ) scope)
fold v a l lrc (Letrec alphas scopes scope) = lrc alphas hScopes hScope
  where
    hScopes = hoistScope (fold v a l lrc) <$> scopes
    hScope = hoistScope (fold v a l lrc) scope

mapAlpha :: (n -> m) -> (Exp n) a -> (Exp m) a
mapAlpha f = fold Var App l lrc
  where
    l a s = Lam (f <$> a) s
    lrc a ss s = Letrec (fmap (fmap f) a) ss s

mapExp :: (n -> m) -> (a -> b) -> Exp n a -> Exp m b
mapExp f g e = mapAlpha f . fmap g $ e

-- | a smart constructor for abstractions
infixl 9 #

(#) :: (Exp n a) -> (Exp n a) -> (Exp n a)
(#) = App

infixr 6 !

(!) :: Eq a => a -> Exp a a -> Exp a a
(!) = lam

lam :: Eq a => a -> Exp a a -> Exp a a

lam a e = gLam a id e

gLam :: Eq a => a -> (a -> n) -> Exp n a -> Exp n a
gLam a f e = Lam (Alpha (f a)) (abstract1 a e)

-- | a smart constructor for let bindings
letrec :: Eq a => [(a, Exp a a)] -> Exp a a -> Exp a a
letrec [] b = b
letrec defs expr = Letrec (Alpha names) (map abstr bodies) (abstr expr)
   where
     abstr = abstract (`elemIndex` names)
     names = map fst defs
     bodies = map snd defs

