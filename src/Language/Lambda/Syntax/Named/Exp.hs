-- {-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Language.Lambda.Syntax.Named.Exp
    (
      Exp (Var,App,Lam,Letrec)
--    , bound
--    , free
    , fold
    , gFold
    , (#)
    , (!)
    ) where

import Data.Char
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Foldable
import Data.Traversable
#endif
import Prelude hiding (abs, fold)
import Text.ParserCombinators.ReadP

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

-- -----------------------------------------------------------------------------
-- named lambda terms

data Exp a
    = Var a
    | App (Exp a) (Exp a)
    | Lam a (Exp a)
    | Letrec [(a, Exp a)] (Exp a)
    deriving (Read, Show, Functor, Foldable, Traversable)

-- folds
fold ::
       (a -> n a)
    -> (n a -> n a -> n a)
    -> (a -> n a -> n a)
    -> ([(a, n a)] -> n a -> n a)
    -> Exp a -> n a
fold v _ _ _ (Var n) = v n
fold v a l ltc (fun `App` arg) = a (fold v a l ltc fun) (fold v a l ltc arg)
fold v a l ltc (Lam n body) = l n (fold v a l ltc body)
fold v a l ltc (Letrec defs term) = ltc ((fmap . fmap) g defs) (g term)
  where
    g = fold v a l ltc

gFold ::
       (m a -> n b)
    -> (n b -> n b -> n b)
    -> (m a -> n b -> n b)
    -> ([(m a, n b)] -> n b -> n b)
    -> Exp (m a) -> n b
gFold v _ _ _ (Var n) = v n
gFold v a l ltc (fun `App` arg) = a (gFold v a l ltc fun) (gFold v a l ltc arg)
gFold v a l ltc (Lam n body) = l n (gFold v a l ltc body)
gFold v a l ltc (Letrec defs expr) = ltc ((fmap . fmap) g defs) (g expr)
  where
    g = gFold v a l ltc


-- constructor

infixl 9 #

(#) :: Exp a -> Exp a -> Exp a
(#) = App

infixr 6 !

(!) :: a -> Exp a -> Exp a
(!) = Lam


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
