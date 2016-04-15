{-# LANGUAGE RankNTypes #-}

module Language.Lambda.Semantics.Named.Internal
    (
      refresh
    , defresh
    , eval
    , compute
    ) where

import qualified Bound.Unwrap as BU
import qualified Language.Lambda.Syntax.Named.Exp as N
import qualified Language.Lambda.Syntax.Nameless.Exp as NL
import Language.Lambda.Syntax.Translation (uname, name)

refresh :: Functor f => f a -> f (BU.Fresh a)
refresh = fmap BU.name

defresh :: Functor f => (BU.Fresh a -> a) -> f (BU.Fresh a) -> f a
defresh g = fmap g

eval :: Eq a =>
       (BU.Fresh a -> a)
    -> (forall n a . NL.Exp n a -> NL.Exp n a)
    -> N.Exp a -> N.Exp a
eval g h = defresh g . compute h . refresh

compute :: Eq a =>
    (forall n a . NL.Exp n a -> NL.Exp n a)
    -> N.Exp (BU.Fresh a)
    -> N.Exp (BU.Fresh a)
compute h = name . h . uname
