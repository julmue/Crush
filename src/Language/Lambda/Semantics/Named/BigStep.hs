module Language.Lambda.Semantics.Named.BigStep
    (
      mkNormalOrder
    , mkCallByName
    , mkCallByValue
    , Printer
    ) where

import Prelude hiding (lookup)

import Language.Lambda.Semantics.Named.Internal
import qualified Language.Lambda.Syntax.Named.Exp as N
import qualified Language.Lambda.Semantics.Nameless.BigStep as NLB

mkNormalOrder :: Eq a => Printer a -> N.Exp a -> N.Exp a
mkNormalOrder p = eval p NLB.normalOrder

mkCallByName :: Eq a => Printer a -> N.Exp a -> N.Exp a
mkCallByName p = eval p NLB.callByName

mkCallByValue :: Eq a => Printer a -> N.Exp a -> N.Exp a
mkCallByValue p = eval p NLB.callByValue
