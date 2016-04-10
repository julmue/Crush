{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Lambda.Semantics.Named.Eval
    (
      normalOrder
    , callByName
    , callByValue
    ) where

import Prelude hiding (lookup)

import Bound
import qualified Bound.Unwrap as BU

import qualified Language.Lambda.Syntax.Named.Exp as N
import qualified Language.Lambda.Syntax.Nameless.Exp as NL
import Language.Lambda.Syntax.Translation (uname, name)
import qualified Language.Lambda.Semantics.Nameless.Eval as NLE

-- -----------------------------------------------------------------------------
-- computation

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

normalOrder :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
normalOrder g = eval g NLE.normalOrder

callByName :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
callByName g = eval g NLE.callByName

callByValue :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
callByValue g = eval g NLE.callByValue
