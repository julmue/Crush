module Lambda (
    -- named lambda terms
      Lambda (Var,Lam,(:@))
    , lambda
    , gLambda
    , (!)
    -- nameless lambda terms
    , NL (V, (:$), L)
    , Alpha (Alpha)
    , runAlpha
    , nl
    , mapAlpha
    , mapNL
    , lam
    , gLam
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
