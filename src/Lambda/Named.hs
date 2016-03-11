{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Named (
      Lambda (Var,Lam,(:@))
    ) where

import Prelude hiding (abs)
import Data.Char
import Text.ParserCombinators.ReadP
-- import Text.Read (readPrec)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Foldable
import Data.Traversable
#endif
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

-- -----------------------------------------------------------------------------
-- named lambda terms

infixl 9 :@

data Lambda a
    = Var a
    | (Lambda a) :@ (Lambda a)
    | Lam a (Lambda a)

-- printing
-- convention:
-- Application has higher precedence than Abstraction
-- Application Associates to the left
-- Lamtraction associates to the right
instance Show a => Show (Lambda a) where
    showsPrec _ (Var n) = showString $ show n
    showsPrec d (Lam n e) = showParen (d>predLam) $
        showChar '\\' . showString (show n) . showChar '.' . showsPrec predLam e where
        predLam = 1
    showsPrec d (e1 :@ e2) = showParen (d>predApp) $
        showsPrec predApp e1 . showChar ' ' . showsPrec (succ predApp) e2 where
        predApp = 2

-- parsing
spaces :: ReadP String
spaces = many1 (satisfy isSpace)
parens :: ReadP a -> ReadP a
parens = between (char '(') (char ')')
varName :: ReadP String
varName = many1 $ satisfy isAlpha
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

-- test data generator
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
