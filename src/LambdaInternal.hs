{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module LambdaInternal (
      Expr (Var,Lam,(:@))
    , T (B, F, L, A)
    , LambdaTerm
    , normalOrder
    , callByName
    , callByValue
    , name
    , uname
    , shift
    , subs
    , beta
    ) where

import Prelude hiding (abs)
import Data.Char
import Data.List
import Data.Foldable
import Text.ParserCombinators.ReadP
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Traversable
#endif
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

-- -----------------------------------------------------------------------------
-- named lambda terms
type Name = String

infixl 9 :@

data Expr
    = Var String
    | Lam String Expr
    | Expr :@ Expr

instance Eq Expr where
    -- equivalence is alpha-conversion
    e1 == e2 = uname e1 == uname e2

-- printing
-- convention:
-- Application has higher precedence than Abstraction
-- Application Associates to the left
-- Lamtraction associates to the right
instance Show Expr where
    showsPrec _ (Var n) = showString n
    showsPrec d (Lam n e) = showParen (d>predLam) $
        showChar '\\' . showString n . showChar '.' . showsPrec predLam e where
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
var :: ReadP Expr
var = Var <$> varName

atom :: ReadP Expr
atom = parens expr <++ var

app :: ReadP Expr
app = atom `chainl1` (spaces *> pure (:@))

abs :: ReadP Expr
abs = Lam <$ lam <*> varName <* dot <*> expr where
    lam = char '\\'
    dot = char '.'

expr :: ReadP Expr
expr = app <++ abs <++ atom

instance Read Expr where
    readsPrec _ = readP_to_S expr

-- test data generator
genExpr :: Int -> Gen Expr
genExpr depth
    | depth <= 1 = Var <$> genNames
    | otherwise = do
        depth1 <- genDepth
        depth2 <- genDepth
        oneof [ genExpr 1
              , (:@) <$> genExpr depth1 <*> genExpr depth2
              , Lam <$> genNames <*> genExpr depth1
              ]
  where
    genNames = elements . fmap return $ "xyz"
    genDepth = elements [1 .. pred depth]

instance Arbitrary Expr where
    arbitrary = sized genExpr

-- -----------------------------------------------------------------------------
-- nameless lambda terms (De Bruijn representation)
data T a
    = B Int
    | F a
    | L (T a)
    | A (T a) (T a)
    deriving (Eq, Functor, Foldable, Traversable)

instance Show a => Show (T a) where
    showsPrec _ (B idx) = showString (show idx)
    showsPrec _ (F n) = showString (show n)
    showsPrec d (L e) = showParen (d > predLam) $
        showChar '\\' . showChar '.' . showsPrec predLam e where
        predLam = 1
    showsPrec d (A e1 e2) = showParen (d>predApp) $
        showsPrec predApp e1 . showChar ' ' . showsPrec (succ predApp) e2 where
        predApp = 2

uname :: Expr -> T String
uname = flip un [] where
    un (Var n) stack = case n `elemIndex` stack of
        Nothing -> F n
        Just idx -> B idx
    un (Lam n e) stack = L (un e (n:stack))
    un (e1 :@ e2) stack = A (un e1 stack) (un e2 stack)

name :: T String -> Expr
name term = nm term [] where
    nm (F n) _ = Var n
    nm (B idx) stack = Var $ stack !! idx
    nm (L t) stack = Lam fresh (nm t (fresh:stack)) where
        fresh = head $ names \\ (stack `union` dom t)
        dom :: T String -> Names
        dom = toList
    nm (A t1 t2) stack = nm t1 stack :@ nm t2 stack

type Names = [Name]

names :: [Name]
names = [ [c] | c <- ['a'..'z']] ++ [c: show j | j <- [1..] :: [Int], c <- ['a'..'z']]

-- shift
shift :: Int -> Int -> T a -> T a
shift _ _ term@(F _) = term
shift delta binders term@(B index) =
    if bound index then term else B (index + delta) where
    bound idx = idx < binders
shift delta binders (L t) = L $ shift delta (succ binders) t
shift delta binders (A t1 t2) = A (shift delta binders t1) (shift delta binders t2)

shiftUp :: T a -> T a
shiftUp = shift 1 0
shiftDown :: T a -> T a
shiftDown = shift (-1) 0

-- substitution
subs :: T a -> Int -> T a -> T a
subs _ _ term@(F _) = term
subs ersatz j term@(B index) = if index == j then ersatz else term
subs ersatz j (L t) = L $ subs (shiftUp ersatz) (succ j) t
subs ersatz j (A t1 t2) = A (subs ersatz j t1) (subs ersatz j t2)

-- beta reduction / computation
beta :: T a -> T a -> T a
beta (L t1) t2 = shiftDown (subs (shiftUp t2) 0 t1)
beta _ _ = error "beta reduction blew up!"

-- -----------------------------------------------------------------------------
-- evaluation
-- 1. full beta-reduction:
--      any redex may be reduced at any time
-- 2. normal-order:
--      leftmost, outermost redex is always reduced first
-- 3. call-by-name:
--      no reductions inside abstractions
-- 4. call-by-value:
--      only outermost redexes are reduced and
--      only if its right-hand side has been reduced to a value

class LambdaTerm t where
    normalOrder :: t -> t
    callByName :: t -> t
    callByValue :: t -> t

instance LambdaTerm (T a) where
    normalOrder = no where
        no b@(B _) = b
        no f@(F _) = f
        no (L t) = L $ no t
        no (A t1 t2) = case no t1 of
            v@(L _) -> no $ beta v (no t2)
            t -> A t t2
    callByName = cbn where
        cbn :: T a -> T a
        cbn b@(B _) = b
        cbn f@(F _) = f
        cbn v@(L _) = v
        cbn (A t1 t2) = case cbn t1 of
            v@(L _) -> cbn $ beta v (cbn t2)
            t -> A t (cbn t2)
    callByValue = cbv where
        cbv :: T a -> T a
        cbv b@(B _) = b
        cbv f@(F _) = f
        cbv v@(L _) = v
        cbv (A t1 t2) = case cbv t1 of
            v1@(L _) -> case cbv t2 of
                v2@(L _) -> cbv $ beta v1 v2
                t2' -> A v1 t2'
            t -> A t t2

instance LambdaTerm Expr where
    normalOrder = name . normalOrder . uname
    callByName = name . callByName . uname
    callByValue = name . callByValue . uname
