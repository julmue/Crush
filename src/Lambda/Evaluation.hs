module Lambda.Evaluation where

import Lambda.Named
import Lambda.Nameless
import Bound

-- smart constructor
lam :: Eq a => a -> Exp a -> Exp a
lam v b = L (abstract1 v b)

infix 0 !
(!) :: Eq a => a -> Exp a -> Exp a
(!) = lam

nf :: Exp a -> Exp a
nf e@V{} = e
nf e@(L b) = L . toScope . nf . fromScope $ b
nf e@(f :$ a) = case whnf f of
    L b -> nf (instantiate1 a b)
    f' -> nf f' :$ nf a

whnf :: Exp a -> Exp a
whnf e@V{} = e
whnf e@L{} = e
whnf (f :$ a) = case whnf f of
    L b -> whnf (instantiate1 a b)
    f' -> f' :$ a
