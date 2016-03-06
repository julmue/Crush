module Lambda (
      Expr (Var,Lam,(:@))
    , LambdaTerm
    , normalOrder
    , callByName
    , callByValue
    ) where

import LambdaInternal
