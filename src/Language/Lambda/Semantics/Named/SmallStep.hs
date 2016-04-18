{-# LANGUAGE RankNTypes #-}

module Language.Lambda.Semantics.Named.SmallStep
    (
      mkNormalOrder
    , mkNormalOrder1
    , mkCallByName
    , mkCallByName1
    , mkCallByValue
    , mkCallByValue1
    ) where

import Prelude hiding (lookup)

import Bound
import qualified Bound.Unwrap as BU

import Language.Lambda.Semantics.Named.Internal
import qualified Language.Lambda.Syntax.Named.Exp as N
import qualified Language.Lambda.Syntax.Nameless.Exp as NL
import qualified Language.Lambda.Semantics.Nameless.SmallStep as NLB

-- -----------------------------------------------------------------------------
-- computation

mkNormalOrder :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
mkNormalOrder g = eval g NLB.normalOrder

mkNormalOrder1 :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
mkNormalOrder1 g = eval g NLB.normalOrder1

mkCallByName :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
mkCallByName g = eval g NLB.callByName

mkCallByName1 :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
mkCallByName1 g = eval g NLB.callByName1

mkCallByValue :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
mkCallByValue g = eval g NLB.callByValue

mkCallByValue1 :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
mkCallByValue1 g = eval g NLB.callByValue1
