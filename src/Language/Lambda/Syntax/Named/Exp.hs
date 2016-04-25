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
    , uname
    , name
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

-- remember to delete this:
import Bound
import Bound.Unwrap (Fresh, Unwrap, unwrap, runUnwrap, freshify)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import qualified Language.Lambda.Syntax.Nameless.Exp as NL

-- -----------------------------------------------------------------------------
-- named lambda terms

data Exp a
    = Var a
    | App (Exp a) (Exp a)                   -- fun arg
    | Lam a (Exp a)                         -- param body
    | Letrec [(a, Exp a)] (Exp a)           -- defs exp
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
-- equality

instance Eq a => Eq (Exp a) where
     l1 == l2 = uname l1 == uname l2

uname :: Eq a => Exp a -> NL.Exp a a
uname = fold NL.Var NL.App NL.lam NL.letrec

name :: Eq a => NL.Exp (Fresh a) (Fresh a) -> Exp (Fresh a)
name = runUnwrap . go
  where
    go :: NL.Exp (Fresh a) (Fresh a) -> Unwrap (Exp (Fresh a))
    go (NL.Var n) = return (Var n)
    go (fun `NL.App` arg) = App <$> (go fun) <*> (go arg)
    go (NL.Lam (NL.Alpha n) scope) = do
        (n', e) <- unwrap n scope
        Lam n' <$> go e
    go (NL.Letrec (NL.Alpha ns) defs scope) = do
        ns' <- sequence . fmap freshify $ ns
        defbodies <- sequence . fmap (go . inst ns') $ defs
        scope' <- go . inst ns'$ scope
        let defs' = zip ns' defbodies
        return (Letrec defs' scope')
      where
        inst :: [Fresh a]
             -> Scope Int (NL.Exp (Fresh a)) (Fresh a)
             -> NL.Exp (Fresh a) (Fresh a)
        inst names = instantiate (dict names)
        dict :: [Fresh a] -> Int -> NL.Exp (Fresh a) (Fresh a)
        dict ns i = NL.Var (ns !! i)


dscopes :: [Scope Int (NL.Exp (Fresh a)) (Fresh a)]
dscopes = undefined

scope :: Scope Int (NL.Exp (Fresh a)) (Fresh a)
scope = undefined

names :: [Fresh a]
names = undefined





-- name lam@Lam{} = runUnwrap (f lam)
--   where
--     f :: NL.Exp (Fresh a) (Fresh a) -> Unwrap (Exp (Fresh a))
--     f (NL.Var n) = return (Var n)
--     f (fun `NL.App` arg) = (App) <$> f fun <*> f arg
--     f (NL.Lam (NL.Alpha n) scope) = (unwrap n scope) >>= g


-- Letrec [(a, Exp a)] (Exp a)
-- Letrec (Alpha [n]) [Scope Int (Exp n) a] (Scope Int (Exp n) a)


-- für die definitionen:
-- ne, dass geht so glaub ich auch nicht ... die definitionen sind ja
-- auch gegenseitig rekursiv ...
-- defs' = zipWith ns defs unwrap
-- defs'' = zip ns defs'

-- für den körper:
-- hier wirds schwer ... es braucht eine funktion die
-- eine listes von namen gleichzeitig in einem scopus instantiert ...

-- g :: (Fresh a, NL.Exp (Fresh a) (Fresh a)) -> Unwrap (Exp (Fresh a))
-- g (n, e) = (Lam n) <$> (f e)


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
