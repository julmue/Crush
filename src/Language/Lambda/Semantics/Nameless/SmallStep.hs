{-# LANGUAGE ScopedTypeVariables #-}

module Language.Lambda.Semantics.Nameless.SmallStep
    (
      normalOrder
    , normalOrder1
    , normalOrderTraced
    , callByName
    , callByName1
    , callByNameTraced
    , callByValue
    , callByValue1
    , callByValueTraced
    ) where

import Prelude hiding (lookup)
import Control.Monad.Writer

import Bound

import Language.Lambda.Syntax.Nameless.Exp
import Language.Lambda.Semantics.Nameless.Internal

data StepResult n a =
      Normalform (Exp n a)
    | Reducible (Exp n a)

foldSR :: (Exp n a -> c) -> (Exp n a -> c) -> StepResult n a -> c
foldSR n _ (Normalform e) = n e
foldSR _ r (Reducible e) = r e

unwrapStepResult :: StepResult n a -> Exp n a
unwrapStepResult = foldSR id id

-- -----------------------------------------------------------------------------
-- Evaluation rules normal-order reduction:
--
--  Congruence rules:
--      t1 -> t1'
--  ----------------- (E-App1)
--   t1 t2 -> t1' t2
--
--      t2 -> t2'
--  ----------------- (E-App2)
--   t1 t2 -> t1 t2'
--
--  Computation rule:
--  (\x.t12) v2 -> [x|->v2] t12 (E-AppAbs)
--
--  Evaluation proceeds under the binder ...
--      t1 -> t1'
--  ------------------
--   (\x.t1) -> \x.t1'

-- small-step normal order evaluation
normalOrder1 :: Exp n a -> Exp n a
normalOrder1 = unwrapStepResult . stepNO

-- multi-step normal order evaluation
normalOrder :: Exp n a -> Exp n a
normalOrder = foldSR id normalOrder . stepNO

normalOrderTraced :: Exp n a -> (Exp n a, [Exp n a])
normalOrderTraced = tracedEval stepNO

stepNO :: Exp n a -> StepResult n a
stepNO (App (Lam _ body) arg) = Reducible (instantiate1 arg body)
stepNO app@(App fun arg) = case stepNO fun of
    Normalform _ -> case stepNO arg of
        Normalform _ -> Normalform app
        Reducible arg' -> Reducible (App fun arg')
    Reducible fun' -> Reducible (App fun' arg)
stepNO fun@(Lam n body) = case stepNO . fromScope $ body of
    Normalform _  ->  Normalform fun
    Reducible body' -> Reducible (Lam n (toScope body'))
stepNO ltc@Letrec{} = Reducible (instantiateLetrec ltc)
stepNO n = Normalform n

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

-- small-step call-by-name evaluation
callByName1 :: Exp n a -> Exp n a
callByName1 = foldSR id id . stepCBN

-- -- multi-step call-by-name evaluation
callByName :: Exp n a -> Exp n a
callByName = foldSR id callByName . stepCBN

callByNameTraced :: Exp n a -> (Exp n a, [Exp n a])
callByNameTraced = tracedEval stepCBN

stepCBN :: Exp n a -> StepResult n a
stepCBN (App (Lam _ body) arg) = Reducible (instantiate1 arg body)
stepCBN app@(App fun arg) = case stepCBN fun of
    Normalform _ -> Normalform app
    Reducible fun' -> Reducible (App fun' arg)
stepCBN ltc@Letrec{} = Reducible (instantiateLetrec ltc)
stepCBN n = Normalform n

-- -- -----------------------------------------------------------------------------
-- {- Evaluation rules Call-By-Value:
--
--     Congruence rule:
--         t1 -> t1'
--     ----------------- (E-App1)
--      t1 t2 -> t1' t2
--
--         t2 -> t2'
--     ----------------- (E-App2)
--      v1 t2 -> v1 t2'
--
--     Computation rule:
--     (\x.t12) v2 -> [x|->v2] t12 (E-AppAbs)
--
-- -}

callByValue1 :: Exp n a -> Exp n a
callByValue1 = foldSR id id . stepCBV

-- multi-step call-by-value evaluation / head normal form
callByValue :: Exp n a -> Exp n a
callByValue = foldSR id callByValue . stepCBV

callByValueTraced :: Exp n a -> (Exp n a, [Exp n a])
callByValueTraced = tracedEval stepCBV

stepCBV :: Exp n a -> StepResult n a
stepCBV (App (Lam _ body) arg@Lam{}) = Reducible (instantiate1 arg body)
stepCBV app@(App fun@Lam{} arg) = case stepCBV arg of
    Normalform _ -> Normalform app
    Reducible arg' -> Reducible (App fun arg')
stepCBV app@(App fun arg) = case stepCBV fun of
    Normalform _ -> Normalform app
    Reducible fun' -> Reducible (App fun' arg)
stepCBV ltc@Letrec{} = Reducible (instantiateLetrec ltc)
stepCBV n = Normalform n

tracedEval :: forall n a . (Exp n a -> StepResult n a) -> Exp n a -> (Exp n a, [Exp n a])
tracedEval step expr = runWriter $ writer (expr,[expr]) >>= go
  where
    go :: Exp n a -> Writer [Exp n a] (Exp n a)
    go e = case step e of
        Normalform e' -> writer (e', mempty)
        Reducible e' -> writer (e', [e']) >>= go
