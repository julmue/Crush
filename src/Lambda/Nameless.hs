{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

module Lambda.Nameless where

import Control.Monad (ap)
import Prelude.Extras
import Bound
import Bound.Scope

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
    deriving (Eq,Show,Read,Functor,Foldable,Traversable)

instance (Eq n) => Eq1 (NL n)
instance (Show n) => Show1 (NL n)
instance (Read n) => Read1 (NL n)

instance Monad (NL n) where
    return = V
    (V a) >>= f = f a
    (fun :$ arg) >>= f = (fun >>= f) :$ (arg >>= f)
    (L n e) >>= f = L n (e >>>= f)

instance Applicative (NL n) where
    pure = V
    (<*>) = ap

nl :: forall n b f .
    (forall a . a -> f a) ->
    (forall a . f a -> f a -> f a) ->
    (forall a . Alpha n -> Scope () f a -> f a) ->
    (NL n) b -> f b
nl v _ _ (V n) = v n
nl v a l (fun :$ arg) = a (nl v a l fun) (nl v a l arg)
nl v a l (L alpha scope) = l alpha (hoistScope (nl v a l) scope)

mapAlpha :: (n -> m) -> (NL n) a -> (NL m) a
mapAlpha f = nl V (:$) (\a s -> L (f <$> a) s)

mapNL :: (n -> m) -> (a -> b) -> NL n a -> NL m b
mapNL f g e = mapAlpha f . fmap g $ e

-- smart constructor
lam :: Eq a => a -> NL a a -> NL a a
lam a e = gLam a id e

gLam :: Eq a => a -> (a -> n) -> NL n a -> NL n a
gLam a f e = L (Alpha (f a)) (abstract1 a e)
