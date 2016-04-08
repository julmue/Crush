-- {-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lambda.Named
    (
      Lambda (Var,(:@),Lam,Letrec)
--    , bound
--    , free
    , lambda
    , gLambda
    , (!)
    ) where

import Data.Char
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Foldable
import Data.Traversable
#endif
import Prelude hiding (abs)
import Text.ParserCombinators.ReadP

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

-- -----------------------------------------------------------------------------
-- named lambda terms

infixl 9 :@

-- what happens?
-- 1. the file gets parsed ...
--    we have a list of [(a,Lambda a)]
-- 2. these get abstracted
--    [Scope Int (Lambda a) a)]
-- 3. these get put in a Vector


data Lambda a
    = Var a
    | (Lambda a) :@ (Lambda a)
    | Lam a (Lambda a)
    | Letrec [(a, Lambda a)] (Lambda a)
    deriving (Functor, Foldable, Traversable)

-- folds
lambda ::
       (a -> n a)
    -> (n a -> n a -> n a)
    -> (a -> n a -> n a)
    -> ([(a, n a)] -> n a -> n a)
    -> Lambda a -> n a
lambda v _ _ _ (Var n) = v n
lambda v a l ltc (fun :@ arg) = a (lambda v a l ltc fun) (lambda v a l ltc arg)
lambda v a l ltc (Lam n body) = l n (lambda v a l ltc body)
lambda v a l ltc (Letrec defs expr) = ltc ((fmap . fmap) g defs) (g expr)
  where
    g = lambda v a l ltc

gLambda ::
       (m a -> n b)
    -> (n b -> n b -> n b)
    -> (m a -> n b -> n b)
    -> ([(m a, n b)] -> n b -> n b)
    -> Lambda (m a) -> n b
gLambda v _ _ _ (Var n) = v n
gLambda v a l ltc (fun :@ arg) = a (gLambda v a l ltc fun) (gLambda v a l ltc arg)
gLambda v a l ltc (Lam n body) = l n (gLambda v a l ltc body)
gLambda v a l ltc (Letrec defs expr) = ltc ((fmap . fmap) g defs) (g expr)
  where
    g = gLambda v a l ltc


-- constructor
infixr 6 !

(!) :: a -> Lambda a -> Lambda a
(!) = Lam

-- -----------------------------------------------------------------------------
-- show instance
instance Show a => Show (Lambda a) where
    showsPrec _ (Var n) = showString (show n)
    showsPrec d (e1 :@ e2) = showParen (d>predApp) $
        showsPrec predApp e1 . showChar ' ' . showsPrec (succ predApp) e2
      where
        predApp = 9
    showsPrec d (Lam n e) = showParen (d>predLam) $
        showChar '\\' .
        showString (show n) .
        showString "->" . showChar ' ' . showsPrec predLam e
      where
        predLam = 6
-- I do not suppose this is right ...
    showsPrec d (Letrec defs expr) = showParen False $
        showString "letrec " .
        showString (show defs) .
        showString "in " .
        showsPrec 9 expr


-- -----------------------------------------------------------------------------
-- read instance
spaces :: ReadP String
spaces = many1 (satisfy isSpace)
parens :: ReadP a -> ReadP a
parens = between (char '(') (char ')')
var :: Read a => ReadP (Lambda a)
var = Var <$> (readS_to_P (readsPrec 10))

atom :: Read a => ReadP (Lambda a)
atom = parens expr <++ var

app :: Read a => ReadP (Lambda a)
app = atom `chainl1` (spaces *> pure (:@))

abs :: Read a => ReadP (Lambda a)
abs = Lam <$ lam <*> (readS_to_P (readsPrec 10)) <* dot <*> expr where
    lam = char '\\'
    dot = char '.'

expr :: Read a => ReadP (Lambda a)
expr = app <++ abs <++ atom

instance Read a => Read (Lambda a) where
    readsPrec _ = readP_to_S expr

-- -----------------------------------------------------------------------------
-- random data generation

genLambda :: Arbitrary a => Int -> Gen (Lambda a)
genLambda depth
    | depth <= 1 = Var <$> arbitrary
    | otherwise = do
        depth1 <- genDepth
        depth2 <- genDepth
        oneof [ genLambda 1
              , (:@) <$> genLambda depth1 <*> genLambda depth2
              , Lam <$> arbitrary <*> genLambda depth1
              ]
  where
    genDepth = elements [1 .. pred depth]

instance Arbitrary a => Arbitrary (Lambda a) where
    arbitrary = sized genLambda
