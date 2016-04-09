module Lambda (
    -- named lambda terms
      Expr (Var, Lam, (:@), Letrec)
    , expr
    , gExpr
    , (!)
    -- nameless lambda terms
    , NL (V, (:$), L, LRC)
    , Alpha (Alpha)
    , runAlpha
    , nl
    , mapAlpha
    , mapNL
    , lam
    , gLam
    , let_
    -- evaluation
    , normalOrder
    , callByName
    , callByValue
    , normalize
    , lazy
    , strict
    , compute
    , hoistFresh
    -- translation
    , uname
    , name
    ) where

import Lambda.Evaluation
import Lambda.Named
import Lambda.Nameless
import Lambda.Translation
