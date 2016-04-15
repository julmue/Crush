{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Lambda.Semantics.Nameless.SmallStep
    (
      normalOrder
    , callByName
    , callByValue
    ) where

import Prelude hiding (lookup)

import Bound
import qualified Bound.Unwrap as BU

import qualified Language.Lambda.Syntax.Nameless.Exp as NL
import Language.Lambda.Syntax.Nameless.Exp (Exp(Var, App, Lam, Letrec))

import Language.Lambda.Syntax.Nameless.Testdata

-- -----------------------------------------------------------------------------
-- Evaluation rules normal-order reduction:
--
--  Congruence rule:
--      t1 -> t1'
--  ----------------- (E-App1)
--   t1 t2 -> t1' t2
--
--  Computation rule:
--  (\x.t12) v2 -> [x|->v2] t12 (E-AppAbs)
--
--  Evaluation proceeds under the binder ...
--      t1 -> t1'
--  ------------------
--   (\x.t1) -> \x.t1'

stepNO :: Exp n a -> Either (Exp n a) (Exp n a)
stepNO app@(App fun@(Lam _ body) arg) = Right (instantiate1 arg body)
stepNO app@(App fun arg) = case stepNO fun of
    Left _ -> Right app
    Right fun' -> Right (App fun' arg)
stepNO fun@(Lam n body) = case stepNO . fromScope $ body of
    Left _ -> Left fun
    Right body' -> Right (Lam n (toScope body'))
stepNO ltc@Letrec{} = Right (instLtc ltc)
stepNO t = Left t

-- small-step normal order evaluation
normalOrder1 :: Exp n a -> Exp n a
normalOrder1 = either id id . stepNO

-- multi-step normal order evaluation
normalOrder :: forall n a . Exp n a -> Exp n a
normalOrder = either id normalOrder . stepNO

-- -----------------------------------------------------------------------------
-- Evaluation rules Call-By-Value:
--
--  Congruence rule:
--      t1 -> t1'
--  ----------------- (E-App1)
--   t1 t2 -> t1' t2
--
--  Computation rule:
--  (\x.t12) v2 -> [x|->v2] t12 (E-AppAbs)

stepCBN :: Exp n a -> Either (Exp n a) (Exp n a)
stepCBN app@(App fun@(Lam _ body) arg) = Right (instantiate1 arg body)
stepCBN app@(App fun arg) = case stepCBN fun of
    Left _ -> Right app
    Right fun' -> Right (App fun' arg)
stepCBN ltc@Letrec{} = Right (instLtc ltc)
stepCBN t = Left t

callByName1 :: Exp n a -> Exp n a
callByName1 = either id id . stepCBN

-- weak head normal form
callByName :: Exp n a -> Exp n a
callByName = either id callByName . stepCBN

-- -----------------------------------------------------------------------------
{- Evaluation rules Call-By-Value:

    Congruence rule:
        t1 -> t1'
    ----------------- (E-App1)
     t1 t2 -> t1' t2

        t2 -> t2'
    ----------------- (E-App2)
     v1 t2 -> v1 t2'

    Computation rule:
    (\x.t12) v2 -> [x|->v2] t12 (E-AppAbs)

-}
stepCBV :: Exp n a -> Either (Exp n a) (Exp n a)
stepCBV (App fun@(Lam n body) arg@Lam{}) = Right (instantiate1 arg body)
stepCBV app@(App fun@Lam{} arg) = case stepCBV arg of
    Left _ -> Left app
    Right arg' -> Right (App fun arg')
stepCBV app@(App fun arg) = case stepCBV fun of
    Left _ -> Right app
    Right fun' -> Right (App fun' arg)
stepCBV ltc@Letrec{} = Right (instLtc ltc)
stepCBV t = Left t

callByValue1 :: Exp n a -> Exp n a
callByValue1 = either id id . stepCBV

-- head normal form
callByValue :: Exp n a -> Exp n a
callByValue = either id callByValue . stepCBV

-- -----------------------------------------------------------------------------
-- this is not total; obviously that doesn't matter in this use case,
-- but can this be enforced by the compiler?
instLtc :: forall n a . Exp n a -> Exp n a
instLtc (Letrec ns defScopes scope) = instDefs scope
  where
    defs = map instDefs defScopes :: [Exp n a]
    instDefs = instantiate lookup :: Scope Int (Exp n) a -> Exp n a
    lookup = (defs !!) :: Int -> Exp n a
