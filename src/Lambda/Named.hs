-- {-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lambda.Named
    (
      Expr (Var,(:@),Lam,Letrec)
--    , bound
--    , free
    , expr
    , gExpr
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
--    we have a list of [(a,Expr a)]
-- 2. these get abstracted
--    [Scope Int (Expr a) a)]
-- 3. these get put in a Vector


data Expr a
    = Var a
    | (Expr a) :@ (Expr a)
    | Lam a (Expr a)
    | Letrec [(a, Expr a)] (Expr a)
    deriving (Functor, Foldable, Traversable)

-- folds
expr ::
       (a -> n a)
    -> (n a -> n a -> n a)
    -> (a -> n a -> n a)
    -> ([(a, n a)] -> n a -> n a)
    -> Expr a -> n a
expr v _ _ _ (Var n) = v n
expr v a l ltc (fun :@ arg) = a (expr v a l ltc fun) (expr v a l ltc arg)
expr v a l ltc (Lam n body) = l n (expr v a l ltc body)
expr v a l ltc (Letrec defs term) = ltc ((fmap . fmap) g defs) (g term)
  where
    g = expr v a l ltc

gExpr ::
       (m a -> n b)
    -> (n b -> n b -> n b)
    -> (m a -> n b -> n b)
    -> ([(m a, n b)] -> n b -> n b)
    -> Expr (m a) -> n b
gExpr v _ _ _ (Var n) = v n
gExpr v a l ltc (fun :@ arg) = a (gExpr v a l ltc fun) (gExpr v a l ltc arg)
gExpr v a l ltc (Lam n body) = l n (gExpr v a l ltc body)
gExpr v a l ltc (Letrec defs expr) = ltc ((fmap . fmap) g defs) (g expr)
  where
    g = gExpr v a l ltc


-- constructor
infixr 6 !

(!) :: a -> Expr a -> Expr a
(!) = Lam

-- -----------------------------------------------------------------------------
-- show instance
instance Show a => Show (Expr a) where
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
-- spaces :: ReadP String
-- spaces = many1 (satisfy isSpace)
-- parens :: ReadP a -> ReadP a
-- parens = between (char '(') (char ')')
-- var :: Read a => ReadP (Expr a)
-- var = Var <$> (readS_to_P (readsPrec 10))
--
-- atom :: Read a => ReadP (Expr a)
-- atom = parens expr <++ var
--
-- app :: Read a => ReadP (Expr a)
-- app = atom `chainl1` (spaces *> pure (:@))
--
-- abs :: Read a => ReadP (Expr a)
-- abs = Lam <$ lam <*> (readS_to_P (readsPrec 10)) <* dot <*> expr where
--     lam = char '\\'
--     dot = char '.'
--
-- expr :: Read a => ReadP (Expr a)
-- expr = app <++ abs <++ atom
--
-- instance Read a => Read (Expr a) where
--     readsPrec _ = readP_to_S expr

-- -----------------------------------------------------------------------------
-- random data generation

genExpr :: Arbitrary a => Int -> Gen (Expr a)
genExpr depth
    | depth <= 1 = Var <$> arbitrary
    | otherwise = do
        depth1 <- genDepth
        depth2 <- genDepth
        oneof [ genExpr 1
              , (:@) <$> genExpr depth1 <*> genExpr depth2
              , Lam <$> arbitrary <*> genExpr depth1
              ]
  where
    genDepth = elements [1 .. pred depth]

instance Arbitrary a => Arbitrary (Expr a) where
    arbitrary = sized genExpr
