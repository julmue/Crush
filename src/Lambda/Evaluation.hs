{-# Language FlexibleInstances #-}

module Lambda.Evaluation where

import Control.Monad.Reader
import Data.List ((\\))

import Lambda.Named
import Lambda.Nameless
import Lambda.Translation

import Bound
import Bound.Unwrap

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


compute :: Eq a => Lambda (Fresh a) -> Lambda (Fresh a)
compute = nm . nf . unm
