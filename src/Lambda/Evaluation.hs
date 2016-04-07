{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Lambda.Evaluation
    (
      normalOrder
    , callByName
    , callByValue
    , compute
    , normalize
    , lazy
    , strict
    , hoistFresh
    ) where

import Prelude hiding (lookup)

import Bound
import qualified Bound.Unwrap as BU

import Lambda.Named
import Lambda.Nameless
import Lambda.Translation

-- -----------------------------------------------------------------------------
-- computation

-- normal form
normalOrder :: forall n a . NL n a -> NL n a
normalOrder e@V{} = e
normalOrder (L n s) = L n . toScope . normalOrder . fromScope $ s
normalOrder (f :$ a) = case callByName f of -- this has to be callByName because it loops otherwise in some cases ... but why?
    L _ b -> normalOrder (instantiate1 a b)
    f' -> normalOrder f' :$ normalOrder a
normalOrder (LRC _ defScopes scope) = normalOrder (instDefs scope)
  where
    defs = map instDefs defScopes :: [NL n a]
    instDefs = instantiate lookup :: Scope Int (NL n) a -> NL n a
    lookup = (defs !!) :: Int -> NL n a

-- weak head normal form
callByName :: forall n a . NL n a -> NL n a
callByName var@V{} = var
callByName val@L{} = val
callByName (fun :$ arg) = case callByName fun of
    L _ body -> callByName (instantiate1 arg body)
    term -> term :$ arg
callByName (LRC ns defScopes scope) = callByName (instDefs scope)
  where
    defs = map instDefs defScopes :: [NL n a]
    instDefs = instantiate lookup :: Scope Int (NL n) a -> NL n a
    lookup = (defs !!) :: Int -> NL n a

-- head normal form
callByValue :: forall n a . NL n a -> NL n a
callByValue var@V{} = var
callByValue val@L{} = val
callByValue (fun :$ arg) = case callByValue fun of
    val@(L _ body) -> case callByValue arg of
        val'@(L _ _) ->  callByValue (instantiate1 val' body)
        term' -> val :$ term'
    term -> term :$ arg
callByValue (LRC ns defScopes scope) = callByValue (instDefs scope)
  where
    defs = map instDefs defScopes :: [NL n a]
    instDefs = instantiate lookup :: Scope Int (NL n) a -> NL n a
    lookup = (defs !!) :: Int -> NL n a

hoistFresh :: Lambda a -> Lambda (BU.Fresh a)
hoistFresh = fmap BU.name

compute :: Eq a =>
    (forall n a . NL n a -> NL n a)
    -> Lambda (BU.Fresh a)
    -> Lambda (BU.Fresh a)
compute f = name . f . uname

normalize :: Eq a => Lambda (BU.Fresh a) -> Lambda (BU.Fresh a)
normalize = compute normalOrder

lazy :: Eq a => Lambda (BU.Fresh a) -> Lambda (BU.Fresh a)
lazy = compute callByName

strict :: Eq a => Lambda (BU.Fresh a) -> Lambda (BU.Fresh a)
strict = compute callByValue
