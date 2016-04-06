{-# LANGUAGE RankNTypes #-}

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

import Bound
import qualified Bound.Unwrap as BU

import Lambda.Named
import Lambda.Nameless
import Lambda.Translation

-- -----------------------------------------------------------------------------
-- computation

-- normal form
normalOrder :: NL n a -> NL n a
normalOrder e@V{} = e
normalOrder e@(L n b) = L n . toScope . normalOrder . fromScope $ b
normalOrder e@(f :$ a) = case normalOrder f of
    L _ b -> normalOrder (instantiate1 a b)
    f' -> normalOrder f' :$ normalOrder a

-- weak head normal form
callByName :: NL n a -> NL n a
callByName var@V{} = var
callByName val@L{} = val
callByName (fun :$ arg) = case callByName fun of
    L _ body -> callByName (instantiate1 arg body)
    term -> term :$ arg

-- head normal form
callByValue :: NL n a -> NL n a
callByValue var@V{} = var
callByValue val@L{} = val
callByValue (fun :$ arg) = case callByValue fun of
    val@(L _ body) -> case callByValue arg of
        val'@(L _ _) ->  callByValue (instantiate1 val' body)
        term' -> val :$ term'
    term -> term :$ arg

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
