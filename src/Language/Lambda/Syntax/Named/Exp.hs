{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Language.Lambda.Syntax.Named.Exp
    (
      Exp (Var,App,Lam,Let)
--    , bound
--    , free
    , uname
    , name
    , fold
    , gFold
    , (#)
    , (!)
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Foldable
import Data.Traversable
#endif

import Bound.Unwrap (Fresh, Unwrap, unwrap, runUnwrap)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import qualified Language.Lambda.Syntax.Nameless.Exp as NL

-- -----------------------------------------------------------------------------
-- named lambda terms

data Exp a
    = Var a
    | App (Exp a) (Exp a)
    | Lam a (Exp a)
    | Let (a, Exp a) (Exp a)
    deriving (Read, Show, Functor, Foldable, Traversable)

fold ::
       (a -> n a)
    -> (n a -> n a -> n a)
    -> (a -> n a -> n a)
    -> ((a, n a) -> n a -> n a)
    -> Exp a -> n a
fold v _ _ _ (Var n) = v n
fold v a l lt (fun `App` arg) = a (fold v a l lt fun) (fold v a l lt arg)
fold v a l lt (Lam n body) = l n (fold v a l lt body)
fold v a l lt (Let def term) = lt (fmap g def) (g term)
  where
    g = fold v a l lt

gFold ::
       (m a -> n b)
    -> (n b -> n b -> n b)
    -> (m a -> n b -> n b)
    -> ((m a, n b) -> n b -> n b)
    -> Exp (m a) -> n b
gFold v _ _ _ (Var n) = v n
gFold v a l lt (fun `App` arg) = a (gFold v a l lt fun) (gFold v a l lt arg)
gFold v a l lt (Lam n body) = l n (gFold v a l lt body)
gFold v a l lt (Let defs expr) = lt (fmap g defs) (g expr)
  where
    g = gFold v a l lt

infixl 9 #
(#) :: Exp a -> Exp a -> Exp a
(#) = App

infixr 6 !
(!) :: a -> Exp a -> Exp a
(!) = Lam

instance Eq a => Eq (Exp a) where
     l1 == l2 = uname l1 == uname l2

uname :: Eq a => Exp a -> NL.Exp a a
uname = fold NL.Var NL.App NL.lam_ NL.let_

name :: Eq a => NL.Exp (Fresh a) (Fresh a) -> Exp (Fresh a)
name = runUnwrap . go
  where
    go :: NL.Exp (Fresh a) (Fresh a) -> Unwrap (Exp (Fresh a))
    go (NL.Var n) = return (Var n)
    go (fun `NL.App` arg) = App <$> (go fun) <*> (go arg)
    go (NL.Lam (NL.Alpha n) scope) = do
        (n', e) <- unwrap n scope
        Lam n' <$> go e
    go (NL.Let (NL.Alpha n) d scope) = do
        d' <- go d
        (n', e) <- unwrap n scope
        (Let (n', d')) <$> go e

-- -----------------------------------------------------------------------------
-- random data generation

genExpr :: Arbitrary a => Int -> Gen (Exp a)
genExpr depth
    | depth <= 1 = Var <$> arbitrary
    | otherwise = do
        depth1 <- genDepth
        depth2 <- genDepth
        oneof [ genExpr 1
              , App <$> genExpr depth1 <*> genExpr depth2
              , Lam <$> arbitrary <*> genExpr depth1
              ]
  where
    genDepth = elements [1 .. pred depth]

instance Arbitrary a => Arbitrary (Exp a) where
    arbitrary = sized genExpr
