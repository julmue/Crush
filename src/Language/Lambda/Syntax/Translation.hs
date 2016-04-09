module Language.Lambda.Syntax.Translation
    (
      uname
    , name
    ) where

import Bound.Unwrap (Fresh, Unwrap, unwrap, runUnwrap)

import qualified Language.Lambda.Syntax.Named.Exp as N
import qualified Language.Lambda.Syntax.Nameless.Exp as NL


uname :: Eq a => N.Exp a -> NL.Exp a a
uname = N.fold NL.Var NL.App NL.lam NL.letrec

name :: Eq a => NL.Exp (Fresh a) (Fresh a) -> N.Exp (Fresh a)
name (NL.Var n) = N.Var n
name (fun `NL.App` arg) = name fun `N.App` name arg
name l = runUnwrap (f l)
  where
    f :: NL.Exp (Fresh a) (Fresh a) -> Unwrap (N.Exp (Fresh a))
    f (NL.Var n) = return (N.Var n)
    f (fun `NL.App` arg) = (N.App) <$> f fun <*> f arg
    f (NL.Lam (NL.Alpha n) scope) = (unwrap n scope) >>= g
    g :: (Fresh a, NL.Exp (Fresh a) (Fresh a)) -> Unwrap (N.Exp (Fresh a))
    g (n, e) = (N.Lam n) <$> (f e)


