module Lambda.Translation where

import Lambda.Named
import Lambda.Nameless
-- import Bound
import Bound.Unwrap

unm :: Eq a => Lambda a -> NL a a
unm = lambda V (:$) (\x e -> x ! e)

nm :: Eq a => NL (Fresh a) (Fresh a) -> Lambda (Fresh a)
nm (V n) = Var n
nm (fun :$ arg) = nm fun :@ nm arg
nm l = runUnwrap (f l)
  where
    f :: NL (Fresh a) (Fresh a) -> Unwrap (Lambda (Fresh a))
    f (V n) = return (Var n)
    f (fun :$ arg) = (:@) <$> f fun <*> f arg
    f (L (Alpha n) scope) = (unwrap n scope) >>= g
    g :: (Fresh a, NL (Fresh a) (Fresh a)) -> Unwrap (Lambda (Fresh a))
    g (n, e) = (Lam n) <$> (f e)


