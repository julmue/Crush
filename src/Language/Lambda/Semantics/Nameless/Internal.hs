{-# LANGUAGE ScopedTypeVariables #-}

module Language.Lambda.Semantics.Nameless.Internal
    (
      instantiateLetrec
    ) where

import Bound
import Language.Lambda.Syntax.Nameless.Exp

-- -----------------------------------------------------------------------------
-- this is not total; obviously that doesn't matter in this use case,
-- but can this be enforced by the compiler?
instantiateLetrec :: forall n a . Exp n a -> Exp n a
instantiateLetrec (Letrec ns defScopes scope) = instDefs scope
  where
    defs = map instDefs defScopes :: [Exp n a]
    instDefs = instantiate lookup :: Scope Int (Exp n) a -> Exp n a
    lookup = (defs !!) :: Int -> Exp n a
