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
    , Printer
    ) where

import Prelude hiding (lookup)

import Language.Lambda.Semantics.Named.Internal
import qualified Language.Lambda.Syntax.Named.Exp as N
import qualified Language.Lambda.Semantics.Nameless.SmallStep as NLB

mkNormalOrder :: Eq a => Printer a -> N.Exp a -> N.Exp a
mkNormalOrder p = eval p NLB.normalOrder

mkNormalOrder1 :: Eq a => Printer a -> N.Exp a -> N.Exp a
mkNormalOrder1 p = eval p NLB.normalOrder1

mkNormalOrderTraced :: Eq a => Printer a -> N.Exp a -> (N.Exp a, [N.Exp a])
mkNormalOrderTraced p = evalTraced p NLB.normalOrderTraced

mkNormalOrderTracedLimit :: Eq a => Printer a -> Int -> N.Exp a -> (N.Exp a, [N.Exp a])
mkNormalOrderTracedLimit p i = evalTraced p (NLB.normalOrderTracedLimit i)

mkCallByName :: Eq a => Printer a -> N.Exp a -> N.Exp a
mkCallByName p = eval p NLB.callByName

mkCallByName1 :: Eq a => Printer a -> N.Exp a -> N.Exp a
mkCallByName1 p = eval p NLB.callByName1

mkCallByNameTraced :: Eq a => Printer a -> N.Exp a -> (N.Exp a, [N.Exp a])
mkCallByNameTraced p = evalTraced p NLB.callByNameTraced

mkCallByNameTracedLimit :: Eq a => Printer a -> Int -> N.Exp a -> (N.Exp a, [N.Exp a])
mkCallByNameTracedLimit p i = evalTraced p (NLB.callByNameTracedLimit i)

mkCallByValue :: Eq a => Printer a -> N.Exp a -> N.Exp a
mkCallByValue p = eval p NLB.callByValue

mkCallByValue1 :: Eq a => Printer a -> N.Exp a -> N.Exp a
mkCallByValue1 p = eval p NLB.callByValue1

mkCallByValueTraced :: Eq a => Printer a -> N.Exp a -> (N.Exp a, [N.Exp a])
mkCallByValueTraced p = evalTraced p NLB.callByValueTraced

mkCallByValueTracedLimit :: Eq a => Printer a -> Int -> N.Exp a -> (N.Exp a, [N.Exp a])
mkCallByValueTracedLimit p i = evalTraced p (NLB.callByValueTracedLimit i)
