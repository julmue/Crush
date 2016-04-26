module Language.Lambda.Semantics.Named.SmallStep
    (
      mkNormalOrder
    , mkNormalOrder1
    , mkNormalOrderTraced
    , mkNormalOrderTracedLimit
    , mkCallByName
    , mkCallByName1
    , mkCallByNameTraced
    , mkCallByNameTracedLimit
    , mkCallByValue
    , mkCallByValue1
    , mkCallByValueTraced
    , mkCallByValueTracedLimit
    ) where

import Prelude hiding (lookup)

import qualified Bound.Unwrap as BU

import Language.Lambda.Semantics.Named.Internal
import qualified Language.Lambda.Syntax.Named.Exp as N
import qualified Language.Lambda.Semantics.Nameless.SmallStep as NLB

mkNormalOrder :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
mkNormalOrder g = eval g NLB.normalOrder

mkNormalOrder1 :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
mkNormalOrder1 g = eval g NLB.normalOrder1

mkNormalOrderTraced :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> (N.Exp a, [N.Exp a])
mkNormalOrderTraced g = evalTraced g NLB.normalOrderTraced

mkNormalOrderTracedLimit :: Eq a => (BU.Fresh a -> a) -> Int -> N.Exp a -> (N.Exp a, [N.Exp a])
mkNormalOrderTracedLimit g i = evalTraced g (NLB.normalOrderTracedLimit i)

mkCallByName :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
mkCallByName g = eval g NLB.callByName

mkCallByName1 :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
mkCallByName1 g = eval g NLB.callByName1

mkCallByNameTraced :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> (N.Exp a, [N.Exp a])
mkCallByNameTraced g = evalTraced g NLB.callByNameTraced

mkCallByNameTracedLimit :: Eq a => (BU.Fresh a -> a) -> Int -> N.Exp a -> (N.Exp a, [N.Exp a])
mkCallByNameTracedLimit g i = evalTraced g (NLB.callByNameTracedLimit i)

mkCallByValue :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
mkCallByValue g = eval g NLB.callByValue

mkCallByValue1 :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
mkCallByValue1 g = eval g NLB.callByValue1

mkCallByValueTraced :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> (N.Exp a, [N.Exp a])
mkCallByValueTraced g = evalTraced g NLB.callByValueTraced

mkCallByValueTracedLimit :: Eq a => (BU.Fresh a -> a) -> Int -> N.Exp a -> (N.Exp a, [N.Exp a])
mkCallByValueTracedLimit g i = evalTraced g (NLB.callByValueTracedLimit i)
