{-# LANGUAGE RankNTypes #-}

module Language.Lambda.Semantics.Named.Internal
    (
      eval
    , evalTraced
    ) where

import Data.Bifunctor

import qualified Bound.Unwrap as BU
import qualified Language.Lambda.Syntax.Named.Exp as N
import qualified Language.Lambda.Syntax.Nameless.Exp as NL

refresh :: Functor f => f a -> f (BU.Fresh a)
refresh = fmap BU.name

defresh :: Functor f => (BU.Fresh a -> a) -> f (BU.Fresh a) -> f a
defresh g = fmap g

eval :: Eq a =>
       (BU.Fresh a -> a)
    -> (forall n b . NL.Exp n b -> NL.Exp n b)
    -> N.Exp a -> N.Exp a
eval g h = defresh g . N.name. h . N.uname . refresh

evalTraced :: Eq a =>
       (BU.Fresh a -> a)
    -> (forall n b . NL.Exp n b -> (NL.Exp n b, [NL.Exp n b]))
    -> N.Exp a -> (N.Exp a, [N.Exp a])
evalTraced g h = bimap nm (map nm) . h . N.uname . refresh
  where
    nm = defresh g . N.name
