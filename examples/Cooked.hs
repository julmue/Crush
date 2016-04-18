import Language.Lambda.Syntax.Named.Exp
import Language.Lambda.Semantics.Named.BigStep

import qualified Bound.Unwrap as BU

main :: IO ()
main = putStrLn . show . normalOrder $ cooked_

cooked_ :: Exp String
cooked_ = Letrec
  [ ("fls",  "f" ! "t" ! Var"f")
  , ("tru",   "f" ! "t" ! Var"t")
  , ("if",     "b" ! "t" ! "f" ! Var"b" # Var"f" # Var"t")
  , ("zro",   "z" ! "s" ! Var"z")
  , ("scc",   "n" ! "z" ! "s" ! Var"s" # Var"n")
  , ("one",    Var"scc" # Var"zro")
  , ("two",    Var"scc" # Var"one")
  , ("three",  Var"scc" # Var"two")
  , ("iszro", "n" ! Var"n" # Var"tru" # ("m" ! Var"fls"))
  , ("const",  "x" ! "y" ! Var"x")
  , ("add",    "x" ! "y" ! Var"x" # Var"y" # ("n" ! Var"scc" # (Var"add" # Var"n" # Var"y")))
  , ("mul",    "x" ! "y" ! Var"x" # Var"zro" # ("n" ! Var"add" # Var"y" # (Var"mul" # Var"n" # Var"y")))
  , ("fac",    "x" ! Var"x" # Var"one" # ("n" ! Var"mul" # Var"x" # (Var"fac" # Var"n")))
  , ("eqnat",  "x" ! "y" ! Var"x" # (Var"y" # Var"tru" # (Var"const" # Var"fls")) # ("x1" ! Var"y" # Var"fls" # ("y1" ! Var"eqnat" # Var"x1" # Var"y1")))
  , ("sumto",  "x" ! Var"x" # Var"zro" # ("n" ! Var"add" # Var"x" # (Var"sumto" # Var"n")))
  , ("n5",     Var"add" # Var"two" # Var"three")
  , ("n6",     Var"add" # Var"three" # Var"three")
  , ("n17",    Var"add" # Var"n6" # (Var"add" # Var"n6" # Var"n5"))
  , ("n37",    Var"scc" # (Var"mul" # Var"n6" # Var"n6"))
  , ("n703",   Var"sumto" # Var"n37")
  , ("n720",   Var"fac" # Var"n6")
  ] (Var"eqnat" # Var"n720" # (Var"add" # Var"n703" # Var"n17"))

-- -----------------------------------------------------------------------------

defresh :: BU.Fresh String -> String
defresh f = BU.uname f ++ show (BU.fresh f)

normalOrder :: Exp String -> Exp String
normalOrder = mkNormalOrder defresh
