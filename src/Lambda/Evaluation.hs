module Lambda.Evaluation where

import Lambda.Named
import Lambda.Nameless
import Bound

unname :: Eq a => Lambda a -> NLL a
unname = foldL V (:$) lam


nf :: NLL a -> NLL a
nf e@V{} = e
nf e@(L b) = L . toScope . nf . fromScope $ b
nf e@(f :$ a) = case whnf f of
    L b -> nf (instantiate1 a b)
    f' -> nf f' :$ nf a

whnf :: NLL a -> NLL a
whnf e@V{} = e
whnf e@L{} = e
whnf (f :$ a) = case whnf f of
    L b -> whnf (instantiate1 a b)
    f' -> f' :$ a
