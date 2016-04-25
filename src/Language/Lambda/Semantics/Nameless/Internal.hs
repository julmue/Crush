{-# LANGUAGE ScopedTypeVariables #-}

module Language.Lambda.Semantics.Nameless.Internal
    (
      instantiateLetrec
    ) where

import Bound

import Language.Lambda.Syntax.Nameless.Exp

instantiateLetrec :: forall n a . Exp n a -> Exp n a
instantiateLetrec (Letrec _ defScopes scope) = instDefs scope
  where
    defs = map instDefs defScopes :: [Exp n a]
    instDefs = instantiate dict :: Scope Int (Exp n) a -> Exp n a
    dict = (defs !!) :: Int -> Exp n a
instantiateLetrec _ = error "instantiateLetrec"
