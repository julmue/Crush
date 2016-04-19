import Language.Lambda.Syntax.Named.Exp
import Language.Lambda.Semantics.Named.BigStep

import qualified Bound.Unwrap as BU

import Language.Lambda.Syntax.Named.Testdata

main :: IO ()
main = putStrLn . show . normalOrder $ example_

example_ :: Exp String
example_ = Letrec [
      -- function
      ("s",         s_)
    , ("k",         k_)
    , ("i",         i_)
    , ("omega",     omega_)
    , ("Omega",     _Omega_)
    , ("fix",       fix_)
      -- logic
    , ("tru",       tru_)
    , ("fls",       fls_)
    , ("if",        if_)
    , ("not",       not_)
    , ("and",       and_)
    , ("or",        or_)
    , ("imp",       imp_)
    , ("iff",       iff_)
      -- arithmetic
    , ("iszro",     iszro_)
    , ("scc",       scc_)
    , ("prd",       prd_)
    , ("add",       add_)
    , ("sub",       sub_)
    , ("mlt",       mlt_)
    , ("pow",       pow_)
    , ("leqnat",    leqnat_)
      -- numbers
    , ("zro",       zro_)
    , ("one",       one_)
    , ("n2",        n2_)
    , ("n3",        n3_)
    , ("n4",        n4_)
    , ("n5",        n5_)
    , ("n6",        n6_)
    , ("n7",        n7_)
    , ("n8",        n8_)
    , ("n9",        n9_)
      -- equality
    , ("eqbool",    eqbool_)
    , ("eqnat",    eqnat_)
    ] (Var"add" # Var"n9" # (Var"mlt" # (Var"mlt" # Var"n3" # Var"n3") # Var"n3"))

defresh :: BU.Fresh String -> String
defresh f = BU.uname f ++ show (BU.fresh f)

normalOrder :: Exp String -> Exp String
normalOrder = mkNormalOrder defresh
