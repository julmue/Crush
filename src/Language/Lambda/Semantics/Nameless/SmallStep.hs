module Language.Lambda.Semantics.Nameless.SmallStep
    (
      normalOrder
    , normalOrder1
    , callByName
    , callByName1
    , callByValue
    , callByValue1
    ) where

import Prelude hiding (lookup)

import Bound
import qualified Bound.Unwrap as BU

import qualified Language.Lambda.Syntax.Nameless.Exp as NL
import Language.Lambda.Syntax.Nameless.Exp (Exp(Var, App, Lam, Letrec))
import Language.Lambda.Semantics.Nameless.Internal

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

-- small-step normal order evaluation
normalOrder1 :: Exp n a -> Exp n a
normalOrder1 = either id id . stepNO

-- multi-step normal order evaluation
normalOrder :: Exp n a -> Exp n a
normalOrder = either id normalOrder . stepNO

stepNO :: Exp n a -> Either (Exp n a) (Exp n a)
stepNO app@(App fun@(Lam _ body) arg) = Right (instantiate1 arg body)
stepNO app@(App fun arg) = case stepNO fun of
    Left _ -> Right app
    Right fun' -> Right (App fun' arg)
stepNO fun@(Lam n body) = case stepNO . fromScope $ body of
    Left _ -> Left fun
    Right body' -> Right (Lam n (toScope body'))
stepNO ltc@Letrec{} = Right (instantiateLetrec ltc)
stepNO t = Left t

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
callByName1 = either id id . stepCBN

-- multi-step call-by-name evaluation
callByName :: Exp n a -> Exp n a
callByName = either id callByName . stepCBN

stepCBN :: Exp n a -> Either (Exp n a) (Exp n a)
stepCBN app@(App fun@(Lam _ body) arg) = Right (instantiate1 arg body)
stepCBN app@(App fun arg) = case stepCBN fun of
    Left _ -> Right app
    Right fun' -> Right (App fun' arg)
stepCBN ltc@Letrec{} = Right (instantiateLetrec ltc)
stepCBN t = Left t

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
callByValue1 :: Exp n a -> Exp n a
callByValue1 = either id id . stepCBV

-- multi-step call-by-value evaluation / head normal form
callByValue :: Exp n a -> Exp n a
callByValue = either id callByValue . stepCBV

stepCBV :: Exp n a -> Either (Exp n a) (Exp n a)
stepCBV (App fun@(Lam n body) arg@Lam{}) = Right (instantiate1 arg body)
stepCBV app@(App fun@Lam{} arg) = case stepCBV arg of
    Left _ -> Left app
    Right arg' -> Right (App fun arg')
stepCBV app@(App fun arg) = case stepCBV fun of
    Left _ -> Right app
    Right fun' -> Right (App fun' arg)
stepCBV ltc@Letrec{} = Right (instantiateLetrec ltc)
stepCBV t = Left t

