{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Language.Lambda.Semantics.Nameless.BigStep
    (
      normalOrder
    , callByName
    , callByValue
    ) where

import Prelude hiding (lookup)

import Bound
import qualified Bound.Unwrap as BU

import Language.Lambda.Syntax.Nameless.Exp

-- -----------------------------------------------------------------------------
-- computation

-- normal form
normalOrder :: forall n a . Exp n a -> Exp n a
normalOrder e@Var{} = e
normalOrder (Lam n s) = Lam n . toScope . normalOrder . fromScope $ s
normalOrder (f `App` a) = case callByName f of
    Lam _ b -> normalOrder (instantiate1 a b)
    f' -> normalOrder f' `App` normalOrder a
normalOrder (Letrec _ defScopes scope) = normalOrder (instDefs scope)
  where
    defs = map instDefs defScopes :: [Exp n a]
    instDefs = instantiate lookup :: Scope Int (Exp n) a -> Exp n a
    lookup = (defs !!) :: Int -> Exp n a

-- weak head normal form
callByName :: forall n a . Exp n a -> Exp n a
callByName var@Var{} = var
callByName val@Lam{} = val
callByName (fun `App` arg) = case callByName fun of
    Lam _ body -> callByName (instantiate1 arg body)
    term -> term `App` arg
callByName (Letrec ns defScopes scope) = callByName (instDefs scope)
  where
    defs = map instDefs defScopes :: [Exp n a]
    instDefs = instantiate lookup :: Scope Int (Exp n) a -> Exp n a
    lookup = (defs !!) :: Int -> Exp n a

-- head normal form
callByValue :: forall n a . Exp n a -> Exp n a
callByValue var@Var{} = var
callByValue val@Lam{} = val
callByValue (fun `App` arg) = case callByValue fun of
    val@(Lam _ body) -> case callByValue arg of
        val'@(Lam _ _) ->  callByValue (instantiate1 val' body)
        term' -> val `App` term'
    term -> term `App` arg
callByValue (Letrec ns defScopes scope) = callByValue (instDefs scope)
  where
    defs = map instDefs defScopes :: [Exp n a]
    instDefs = instantiate lookup :: Scope Int (Exp n) a -> Exp n a
    lookup = (defs !!) :: Int -> Exp n a
