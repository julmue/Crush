module Lambda.LambdaLib where

import Lambda.Named

s = "f" ! "g" ! "x" ! Var "f" :@ Var "x" :@ (Var "g" :@ Var "x")
k = "x" ! "y" ! Var "x"
i = "x" ! Var "x"
