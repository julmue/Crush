{-# LANGUAGE RankNTypes #-}

module Language.Lambda.Semantics.Named.Internal
    (
      eval
    , evalTraced
    , Printer
    ) where

import Data.Bifunctor

import qualified Bound.Unwrap as BU
import qualified Language.Lambda.Syntax.Named.Exp as N
import qualified Language.Lambda.Syntax.Nameless.Exp as NL

type Printer a = (a, Int) -> a

refresh :: Functor f => f a -> f (BU.Fresh a)
refresh = fmap BU.name

defresh :: Functor f => (BU.Fresh a -> a) -> f (BU.Fresh a) -> f a
defresh g = fmap g

eval :: Eq a =>
       Printer a
    -> (forall n b . NL.Exp n b -> NL.Exp n b)
    -> N.Exp a -> N.Exp a
eval p h = defresh (refreshPrinter p) . N.name. h . N.uname . refresh
  where

evalTraced :: Eq a =>
        Printer a
    -> (forall n b . NL.Exp n b -> (NL.Exp n b, [NL.Exp n b]))
    -> N.Exp a -> (N.Exp a, [N.Exp a])
evalTraced p h = bimap nm (map nm) . h . N.uname . refresh
  where
    nm = defresh (refreshPrinter p) . N.name

refreshPrinter :: Printer a -> BU.Fresh a -> a
refreshPrinter p fr = p (BU.uname fr, BU.fresh fr)
