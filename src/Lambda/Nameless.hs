{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

module Lambda.Nameless (
      Alpha (Alpha)
    , runAlpha
    , NL (V, (:$), L, LRC)
--    , bound
--    , free
    , nl
    , mapNL
    , mapAlpha
    , lam
    , gLam
    , let_
) where

import Control.Monad (ap)
import Data.List (elemIndex)

import Bound
import Bound.Scope
import Prelude.Extras

-- -----------------------------------------------------------------------------
-- nameless representation

-- For later use : Information object
-- data Info a = I { unInfo :: a}

-- the name of the binder that got abstracted;
-- this information is not relevant for alpha-equivalence

data Alpha n = Alpha { runAlpha :: n }
    deriving (Show, Read, Functor, Foldable, Traversable)

instance Eq n => Eq (Alpha n) where
    _ == _ = True

infixl 9 :$

data NL n a =
      V a
    | NL n a :$ NL n a
    | L (Alpha n) (Scope () (NL n) a)
    | LRC (Alpha [n]) [Scope Int (NL n) a] (Scope Int (NL n) a)
    deriving (Eq,Show,Read,Functor,Foldable,Traversable)

instance (Eq n) => Eq1 (NL n)
instance (Show n) => Show1 (NL n)
instance (Read n) => Read1 (NL n)

instance Monad (NL n) where
    return = V
--  (>>=) :: NL n a -> (a -> NL n b) -> NL n B
    (V a) >>= f = f a
    (fun :$ arg) >>= f = (fun >>= f) :$ (arg >>= f)
    (L n scope) >>= f = L n (scope >>>= f)
    (LRC n bs scope) >>= f = LRC n (map (>>>= f) bs) (scope >>>= f)

instance Applicative (NL n) where
    pure = V
    (<*>) = ap

nl :: forall n b f .
       (forall a . a -> f a)
    -> (forall a . f a -> f a -> f a)
    -> (forall a . Alpha n -> Scope () f a -> f a)
    -> (forall a . Alpha [n] -> [Scope Int f a] -> (Scope Int f a) -> f a)
    -> (NL n) b -> f b
nl v _ _ _ (V n) = v n
nl v a l lrc (fun :$ arg) = a (nl v a l lrc fun) (nl v a l lrc arg)
nl v a l lrc (L alpha scope) = l alpha hScope
  where hScope = (hoistScope (nl v a l lrc ) scope)
nl v a l lrc (LRC alphas scopes scope) = lrc alphas hScopes hScope
  where
    hScopes = hoistScope (nl v a l lrc) <$> scopes
    hScope = hoistScope (nl v a l lrc) scope

mapAlpha :: (n -> m) -> (NL n) a -> (NL m) a
mapAlpha f = nl V (:$) l lrc
  where
    l a s = L (f <$> a) s
    lrc a ss s = LRC (fmap (fmap f) a) ss s

mapNL :: (n -> m) -> (a -> b) -> NL n a -> NL m b
mapNL f g e = mapAlpha f . fmap g $ e

-- | a smart constructor for abstractions
lam :: Eq a => a -> NL a a -> NL a a
lam a e = gLam a id e

gLam :: Eq a => a -> (a -> n) -> NL n a -> NL n a
gLam a f e = L (Alpha (f a)) (abstract1 a e)

-- | a smart constructor for let bindings
let_ :: Eq a => [(a, NL a a)] -> NL a a -> NL a a
let_ [] b = b
let_ defs expr = LRC (Alpha names) (map abstr bodies) (abstr expr)
   where
     abstr = abstract (`elemIndex` names)
     names = map fst defs
     bodies = map snd defs

