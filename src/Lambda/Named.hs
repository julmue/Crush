{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lambda.Named
    (
      Lambda (Var,Lam,(:@))
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

data Lambda a
    = Var a
    | (Lambda a) :@ (Lambda a)
    | Lam a (Lambda a)
    deriving (Functor, Foldable, Traversable)

-- folds
lambda :: (a -> b) -> (b -> b -> b) -> (a -> b -> b) -> Lambda a -> b
lambda v _ _ (Var n) = v n
lambda v a l (fun :@ arg) = a (lambda v a l fun) (lambda v a l arg)
lambda v a l (Lam n body) = l n (lambda v a l body)

gLambda ::
    (m a -> n b) -> (n b -> n b -> n b) ->
    (m a -> n b -> n b) ->
    Lambda (m a) -> n b
gLambda v _ _ (Var n) = v n
gLambda v a l (fun :@ arg) = a (gLambda v a l fun) (gLambda v a l arg)
gLambda v a l (Lam n body) = l n (gLambda v a l body)

-- constructor
infixr 6 !

(!) :: a -> Lambda a -> Lambda a
(!) = Lam

-- -----------------------------------------------------------------------------
-- show instance
instance Show a => Show (Lambda a) where
    showsPrec _ (Var n) = showString (show n)
    showsPrec d (Lam n e) = showParen (d>predLam) $
        showChar '\\' .
        showString (show n) .
        showString "->" . showChar ' ' . showsPrec predLam e
      where
        predLam = 1
    showsPrec d (e1 :@ e2) = showParen (d>predApp) $
        showsPrec predApp e1 . showChar ' ' . showsPrec (succ predApp) e2
      where
        predApp = 2

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
