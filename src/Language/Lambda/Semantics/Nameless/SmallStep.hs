{-# LANGUAGE ScopedTypeVariables #-}

module Language.Lambda.Semantics.Nameless.SmallStep
    (
      normalOrder
    , normalOrder1
    , normalOrderTraced
    , normalOrderTracedLimit
    , callByName
    , callByName1
    , callByNameTraced
    , callByNameTracedLimit
    , callByValue
    , callByValue1
    , callByValueTraced
    , callByValueTracedLimit
    ) where

import Prelude hiding (lookup)
import Control.Monad.Writer

import Bound

import Language.Lambda.Syntax.Nameless.Exp

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
--
--  Evaluation rule for let-expressions
--
--     let x=v1 in t2
--  -------------------- (E-LetV)
--      [x -> v1]t2
--
-- -----------------------------------------------------------------------------

-- small-step normal order evaluation
normalOrder1 :: Exp n a -> Exp n a
normalOrder1 = unwrapStepResult . stepNO

-- multi-step normal order evaluation
normalOrder :: Exp n a -> Exp n a
normalOrder = foldSR id normalOrder . stepNO

normalOrderTraced :: Exp n a -> (Exp n a, [Exp n a])
normalOrderTraced = tracedEval stepNO

normalOrderTracedLimit :: Int -> Exp n a -> (Exp n a, [Exp n a])
normalOrderTracedLimit = tracedEvalLimit stepNO

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
stepNO (Let _ d e) = Reducible (instantiate1 d e)
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
--
--  Evaluation rule for let-expressions
--
--     let x=v1 in t2
--  -------------------- (E-LetV)
--      [x -> v1]t2
--
-- -----------------------------------------------------------------------------

-- small-step call-by-name evaluation
callByName1 :: Exp n a -> Exp n a
callByName1 = foldSR id id . stepCBN

-- -- multi-step call-by-name evaluation
callByName :: Exp n a -> Exp n a
callByName = foldSR id callByName . stepCBN

callByNameTraced :: Exp n a -> (Exp n a, [Exp n a])
callByNameTraced = tracedEval stepCBN

callByNameTracedLimit :: Int -> Exp n a -> (Exp n a, [Exp n a])
callByNameTracedLimit = tracedEvalLimit stepCBN

stepCBN :: Exp n a -> StepResult n a
stepCBN (App (Lam _ body) arg) = Reducible (instantiate1 arg body)
stepCBN app@(App fun arg) = case stepCBN fun of
    Normalform _ -> Normalform app
    Reducible fun' -> Reducible (App fun' arg)
stepCBN (Let _ d e) = Reducible (instantiate1 d e)
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
--  -- Let-Expressions
--
--                t1 -> t1'
--  ------------------------------------ (E-Let)
--    let x=t1 in t2 -> let x=t1' in t2
--
-- -}

callByValue1 :: Exp n a -> Exp n a
callByValue1 = foldSR id id . stepCBV

-- multi-step call-by-value evaluation / head normal form
callByValue :: Exp n a -> Exp n a
callByValue = foldSR id callByValue . stepCBV

callByValueTraced :: Exp n a -> (Exp n a, [Exp n a])
callByValueTraced = tracedEval stepCBV

callByValueTracedLimit :: Int -> Exp n a -> (Exp n a, [Exp n a])
callByValueTracedLimit = tracedEvalLimit stepCBV

stepCBV :: Exp n a -> StepResult n a
stepCBV (App (Lam _ body) arg@Lam{}) = Reducible (instantiate1 arg body)
stepCBV app@(App fun@Lam{} arg) = case stepCBV arg of
    Normalform _ -> Normalform app
    Reducible arg' -> Reducible (App fun arg')
stepCBV app@(App fun arg) = case stepCBV fun of
    Normalform _ -> Normalform app
    Reducible fun' -> Reducible (App fun' arg)
stepCBV (Let _ d@Let{} e) = Reducible (instantiate1 d e)
stepCBV lt@(Let n d e) = case stepCBV d of
    Normalform _ -> Normalform lt
    Reducible d' -> Reducible (Let n d' e)
stepCBV n = Normalform n

tracedEval :: forall n a . (Exp n a -> StepResult n a) -> Exp n a -> (Exp n a, [Exp n a])
tracedEval step expr = runWriter $ writer (expr,[expr]) >>= go
  where
    go :: Exp n a -> Writer [Exp n a] (Exp n a)
    go e = case step e of
        Normalform e' -> writer (e', mempty)
        Reducible e' -> writer (e', [e']) >>= go

tracedEvalLimit :: forall n a . (Exp n a -> StepResult n a) -> Int -> Exp n a -> (Exp n a, [Exp n a])
tracedEvalLimit step counter expr = runWriter $ writer (expr,[expr]) >>= go counter
  where
    go :: Int -> Exp n a -> Writer [Exp n a] (Exp n a)
    go i e =
        if i > 0
        then case step e of
            Normalform e' -> writer (e', mempty)
            Reducible e' -> writer (e', [e']) >>= go (pred i)
        else writer (e, mempty)

