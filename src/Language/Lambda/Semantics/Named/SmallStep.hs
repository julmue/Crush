{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Lambda.Semantics.Named.SmallStep
    (
      normalOrder
    , normalOrder1
    , callByName
    , callByName1
    , callByValue
    , callByValue1
    ) where

import Prelude hiding (lookup)

import Bound
import qualified Bound.Unwrap as BU

import Language.Lambda.Semantics.Named.Internal
import qualified Language.Lambda.Syntax.Named.Exp as N
import qualified Language.Lambda.Syntax.Nameless.Exp as NL
import Language.Lambda.Syntax.Translation (uname, name)
import qualified Language.Lambda.Semantics.Nameless.SmallStep as NLB

-- -----------------------------------------------------------------------------
-- computation

normalOrder :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
normalOrder g = eval g NLB.normalOrder

normalOrder1 :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
normalOrder1 g = eval g NLB.normalOrder1

callByName :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
callByName g = eval g NLB.callByName

callByName1 :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
callByName1 g = eval g NLB.callByName1

callByValue :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
callByValue g = eval g NLB.callByValue

callByValue1 :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
callByValue1 g = eval g NLB.callByValue1
