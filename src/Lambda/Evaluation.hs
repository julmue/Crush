{-# Language FlexibleInstances #-}

module Lambda.Evaluation
    (
      nf
    , whnf
    , compute
    ) where

import Bound
import qualified Bound.Unwrap as BU

import Lambda.Named
import Lambda.Nameless
import Lambda.Translation

-- -----------------------------------------------------------------------------
-- computation

-- normalform
nf :: NL n a -> NL n a
nf e@V{} = e
nf e@(L n b) = L n . toScope . nf . fromScope $ b
nf e@(f :$ a) = case whnf f of
    L _ b -> nf (instantiate1 a b)
    f' -> nf f' :$ nf a

-- weak head normalform
whnf :: NL n a -> NL n a
whnf e@V{} = e
whnf e@L{} = e
whnf (f :$ a) = case whnf f of
    L _ b -> whnf (instantiate1 a b)
    f' -> f' :$ a

hoistFresh :: Lambda a -> Lambda (BU.Fresh a)
hoistFresh = fmap BU.name

compute :: Eq a => Lambda (BU.Fresh a) -> Lambda (BU.Fresh a)
compute = name . nf . uname
