module Lambda.Translation
    (
      uname
    , name
    ) where

import Bound.Unwrap (Fresh, Unwrap, unwrap, runUnwrap)

import Lambda.Named
import Lambda.Nameless

uname :: Eq a => Lambda a -> NL a a
uname = lambda V (:$) (\x e -> lam x e) undefined --attention

name :: Eq a => NL (Fresh a) (Fresh a) -> Lambda (Fresh a)
name (V n) = Var n
name (fun :$ arg) = name fun :@ name arg
name l = runUnwrap (f l)
  where
    f :: NL (Fresh a) (Fresh a) -> Unwrap (Lambda (Fresh a))
    f (V n) = return (Var n)
    f (fun :$ arg) = (:@) <$> f fun <*> f arg
    f (L (Alpha n) scope) = (unwrap n scope) >>= g
    g :: (Fresh a, NL (Fresh a) (Fresh a)) -> Unwrap (Lambda (Fresh a))
    g (n, e) = (Lam n) <$> (f e)


