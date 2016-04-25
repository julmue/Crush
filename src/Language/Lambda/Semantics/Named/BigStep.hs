module Language.Lambda.Semantics.Named.BigStep
    (
      mkNormalOrder
    , mkCallByName
    , mkCallByValue
    ) where

import Prelude hiding (lookup)

import qualified Bound.Unwrap as BU

import Language.Lambda.Semantics.Named.Internal
import qualified Language.Lambda.Syntax.Named.Exp as N
import qualified Language.Lambda.Semantics.Nameless.BigStep as NLB

-- -----------------------------------------------------------------------------
-- computation


mkNormalOrder :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
mkNormalOrder g = eval g NLB.normalOrder

mkCallByName :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
mkCallByName g = eval g NLB.callByName

mkCallByValue :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
mkCallByValue g = eval g NLB.callByValue
