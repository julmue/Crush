module Language.Lambda.Semantics.Named.BigStep
    (
      normalOrder
    , callByName
    , callByValue
    ) where

import Prelude hiding (lookup)

import Bound
import qualified Bound.Unwrap as BU

import Language.Lambda.Semantics.Named.Internal
import qualified Language.Lambda.Syntax.Named.Exp as N
import qualified Language.Lambda.Syntax.Nameless.Exp as NL
import Language.Lambda.Syntax.Translation (uname, name)
import qualified Language.Lambda.Semantics.Nameless.BigStep as NLB

-- -----------------------------------------------------------------------------
-- computation


normalOrder :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
normalOrder g = eval g NLB.normalOrder

callByName :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
callByName g = eval g NLB.callByName

callByValue :: Eq a => (BU.Fresh a -> a) -> N.Exp a -> N.Exp a
callByValue g = eval g NLB.callByValue
