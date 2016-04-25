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
    , Exp (Var, App, Lam, Letrec)
--    , bound
--    , free
--    , fold
--    , mapExp
--    , mapAlpha
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
      Var !a
    -- fun arg
    | App (Exp n a) (Exp n a)
    -- alpha scope
    | Lam (Alpha n) (Scope () (Exp n) a)
    -- alphas defs scope
    | Letrec (Alpha [n]) [Scope Int (Exp n) a] (Scope Int (Exp n) a)
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
    (Letrec ns ds e) >>= g = Letrec ns (map (>>>= g) ds) (e >>>= g)

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
(!) = lam

lam :: Eq a => a -> Exp a a -> Exp a a

lam a e = gLam a id e

gLam :: Eq a => a -> (a -> n) -> Exp n a -> Exp n a
gLam a f e = Lam (Alpha (f a)) (abstract1 a e)

-- | a smart constructor for let bindings
letrec :: Eq a => [(a, Exp a a)] -> Exp a a -> Exp a a
letrec [] b = b
letrec ds e = Letrec (Alpha names) (map abstr bodies) (abstr e)
   where
     abstr = abstract (`elemIndex` names)
     names = map fst ds
     bodies = map snd ds

