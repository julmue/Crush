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
    , nf
    , whnf
    , compute
    -- translation
    , uname
    , name
    ) where

import Lambda.Evaluation
import Lambda.Named
import Lambda.Nameless
import Lambda.Translation
