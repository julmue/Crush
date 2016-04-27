{-# LANGUAGE FlexibleContexts #-}

module Language.Lambda.Semantics.Named.BigStep.Tests
    (
      bigStepTests
    ) where

import Prelude hiding (fromInteger, toInteger)
import Data.Char

import Bound.Unwrap as BU

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.SmallCheck.Series as SC
import Test.Tasty.HUnit

import Language.Lambda.Syntax.Named.Exp
import Language.Lambda.Semantics.Named.BigStep
import Language.Lambda.Syntax.Named.Testdata

renderFresh :: Printer String
renderFresh (n,i) = n ++ show i

normalOrder = mkNormalOrder renderFresh
callByValue = mkCallByValue renderFresh
callByName =  mkCallByName renderFresh

bigStepTests = testGroup "tests" [
      identityTests
    , logicTests
    , arithmeticTests
    , letrecTests
    ]

-- -----------------------------------------------------------------------------
identityTests = testGroup "IdentityTests"
    [ testCase "i_ 1" $
        normalOrder i_ @=? i_
    , testCase "i_ 2" $
        normalOrder (i_ # i_) @=? i_
    , testCase "i_ 3" $
        normalOrder (i_ # k_) @=? k_
    ]

-- -----------------------------------------------------------------------------
logicTests = testGroup "LogicTests"
    [ if_Tests
    , not_Tests
    , and_Tests
    , or_Tests
    , imp_Tests
    , iff_Tests
    , logicLawsTests
    ]

if_Tests = testGroup "if_Tests"
    [ testCase "if_ 1" $
        one_ @=? normalOrder (if_ # tru_ # one_ # zro_)
    , testCase "if_ 2" $
        zro_ @=? normalOrder (if_ # fls_ # one_ # zro_)
    ]

not_Tests = testGroup "not_Tests"
    [ testCase "not_ 1" $
        fls_ @=? normalOrder (not_ # tru_)
    , testCase "not_ 2" $
        tru_ @=? normalOrder (not_ # fls_)
    ]

and_Tests = testGroup "and_Tests"
    [ testCase "and_ 1" $
        tru_ @=? normalOrder (and_ # tru_ # tru_)
    , testCase "and_ 2" $
        fls_ @=? normalOrder (and_ # tru_ # fls_)
    , testCase "and_ 3" $
        fls_ @=? normalOrder (and_ # fls_ # tru_)
    , testCase "and_4 " $
        fls_ @=? normalOrder (and_ # fls_ # fls_)
    ]

or_Tests = testGroup "or_Tests"
    [ testCase "or_ 1" $
        tru_ @=? normalOrder (or_ # tru_ # tru_)
    , testCase "or_ 2" $
        tru_ @=? normalOrder (or_ # tru_ # fls_)
    , testCase "or_ 3" $
        tru_ @=? normalOrder (or_ # fls_ # tru_)
    , testCase "or_ 4" $
        fls_ @=? normalOrder (or_ # fls_ # fls_)
    ]

imp_Tests = testGroup "imp_Tests"
    [ testCase "imp_ 1" $
        tru_ @=? normalOrder (imp_ # tru_ # tru_)
    , testCase "imp_ 2" $
        fls_ @=? normalOrder (imp_ # tru_ # fls_)
    , testCase "imp_ 3" $
        tru_ @=? normalOrder (imp_ # fls_ # tru_)
    , testCase "imp_ " $
        tru_ @=? normalOrder (imp_ # fls_ # fls_)
    ]

iff_Tests = testGroup "iff_Tests"
    [ testCase "iff_ 1" $
        tru_ @=? normalOrder (iff_ # tru_ # tru_)
    , testCase "iff_ 2" $
        fls_ @=? normalOrder (iff_ # tru_ # fls_)
    , testCase "iff_ 3" $
        fls_ @=? normalOrder (iff_ # fls_ # tru_)
    , testCase "iff_ 4" $
        tru_ @=? normalOrder (iff_ # fls_ # fls_)
    ]

logicLawsTests = testGroup "Laws of Logic"
    [ deMorganTests
    ]

deMorganTests = testGroup "De Morgan"
    [ SC.testProperty "De Morgan 1" $
        \p q ->
            let p_ = fromBool p
                q_ = fromBool q
            in normalOrder (not_ # (and_ # p_ # q_)) ==
               normalOrder (or_ # (not_ # p_) # (not_ # q_))
    , SC.testProperty "De Morgan 2" $
        \p q ->
            let p_ = fromBool p
                q_ = fromBool q
            in normalOrder (not_ # (or_ # p_ # q_)) ==
               normalOrder (and_ # (not_ # p_) # (not_ # q_))
    ]


-- -----------------------------------------------------------------------------
arithmeticTests = testGroup "ArithmeticTests"
    [ iszro_Tests
    , scc_Tests
    , prd_Tests
    , scc_prd_RelationTests
    , pls_Tests
    , mlt_Tests
    , add_mlt_RelationTests
    , pow_Tests
    , leqnat_Tests
    , eqnat_Tests
    , fac_Tests
    ]

iszro_Tests = testGroup "iszro_Tests"
    [ testCase "iszro_ 1" $
        normalOrder (iszro_ # zro_) @=? tru_
    , testCase "iszro_ 2" $
        normalOrder (iszro_ # (scc_ # zro_)) @=? fls_
    ]

scc_Tests = testGroup "scc_Tests"
    [ testCase "scc_ 1" $
        normalOrder (scc_ # zro_) @=? one_
    , QC.testProperty "scc_ 2" $
        QC.forAll (interval 0 250) $
            \n -> normalOrder (scc_ # (unsafeFromInt n)) == unsafeFromInt (succ n)
    , scProp "scc_ 3" 100 $
        \n -> let pos = SC.getNonNegative n :: Int
        in normalOrder (scc_ # (unsafeFromInt pos))
           == (unsafeFromInt (succ pos))
    ]

prd_Tests = testGroup "prd_Tests"
    [ testCase "prd_ 1" $
        normalOrder (prd_ # zro_) @=? zro_
    , testCase "prd_ 2" $
        normalOrder (prd_ # one_) @=? zro_
    ]

scc_prd_RelationTests = testGroup "scc_prd_RelationTests"
    [ QC.testProperty "scc_ prd_ 1 (inverse)" $
        QC.forAll (interval 0 250) $
            \n -> let cn = unsafeFromInt n
                  in normalOrder (prd_ # (scc_ # cn)) == cn
    , scProp "scc_ prd_ 2 (inverse)" 50 $
        \n -> let cn = fromPositive n
              in  normalOrder (prd_ # (scc_ # cn)) == cn
    , scProp "scc_ prd_ 3 (inverse)" 50 $
        \n -> let cn = fromPositive n
              in  normalOrder (scc_ # (prd_ # cn)) == cn
    ]

pls_Tests = testGroup "pls_Tests"
    [ scProp "pls_ 1 (`zro_` right neutral)" 50 $
        \n -> let cn = fromPositive n
              in  normalOrder (add_ # cn # zro_) == cn
    , scProp "pls_ 2 (`zro_` left neutral)" 50 $
        \n -> let cn = fromPositive n
              in  normalOrder (add_ # zro_ # cn) == cn
    , scProp "sub_ 1 (`zro_` right neutral)" 50 $
        \n -> let cn = fromPositive n
              in  normalOrder (sub_ # cn # zro_) == cn
    , scProp "add_ sub_ (inverse)" 30 $
        \n -> let cn = fromPositive n
              in  normalOrder (sub_ # cn # (add_ # cn # zro_)) == zro_
    ]

mlt_Tests = testGroup "mtl_Tests"
    [ scProp "mlt_ 1" 30 $
        \n -> let cn = fromPositive n
              in  normalOrder (mlt_ # cn # zro_)  == zro_
    , scProp "mlt_ 2" 30 $
        \n -> let cn = fromPositive n
              in  normalOrder (mlt_ # zro_ # cn)  == zro_
    ]

add_mlt_RelationTests =  testGroup "add_mlt_RelationTests"
    [ scProp "add_ mlt_ (distributivity)" 3 $
        \n m o -> let cn = fromPositive n
                      cm = fromPositive m
                      co = fromPositive o
              in normalOrder (mlt_ # (add_ # cn # cm) # co)
                 == normalOrder (add_ # (mlt_ # cn # co) # (mlt_ # cm # co))
    ]

pow_Tests = testGroup "pow_Tests"
    [ scProp "pow 1" 10 $
        \n -> let cn = fromPositive n
              in normalOrder (pow_ # cn # zro_) == one_
   , scProp "pow 2" 10 $
       \n -> let cn = fromPositive n
             in normalOrder (pow_ # cn # one_ )
                == cn
   , scProp "pow 3" 4 $
       \n -> let cn = fromPositive n
                 two_ = scc_ # one_
             in normalOrder (pow_ # cn # two_ )
                == normalOrder (mlt_ # cn # cn)
    ]

leqnat_Tests = testGroup "leqnat_Tests"
   [ scProp "leqnat 1: reflexivity" 10 $
       \n -> let cn = fromPositive n
             in normalOrder (leqnat_ # cn # cn )
                == tru_
   , scProp "leqnat 2: antisymmetry" 3 $
       \n m -> let cn = fromPositive n
                   cm = fromPositive m
                   premis = and_ # (leqnat_ # cn # cm ) # (leqnat_ # cm # cn)
                   conclusion = fromBool (cn == cm)
               in normalOrder (imp_ # premis # conclusion) == tru_
   , scProp "leqnat 3: transitivity" 3 $
       \n m o -> let cn = fromPositive n
                     cm = fromPositive m
                     co = fromPositive o
                     premis = and_ # (leqnat_ # cn # cm ) # (leqnat_ # cm # co)
                     conclusion = leqnat_ # cn # cm
               in normalOrder (imp_ # premis # conclusion) == tru_
    ]

eqnat_Tests = testGroup "eqnat_Tests"
   [ scProp "eqnat 1 (reflexivity)" 10 $
       \n -> let cn = fromPositive n
             in normalOrder (eqnat_ # cn # cn )
                == tru_
   , scProp "eqnat 2 (symmetry)" 3 $
       \n m -> let cn = fromPositive n
                   cm = fromPositive m
                   premis = eqnat_ # cn # cm
                   conclusion = eqnat_ # cm # cn
               in normalOrder (imp_ # premis # conclusion) == tru_
   , scProp "eqnat 3 (transitivity)" 3 $
       \n m o -> let cn = fromPositive n
                     cm = fromPositive m
                     co = fromPositive o
                     premis = and_ # (eqnat_ # cn # cm ) # (eqnat_ # cm # co)
                     conclusion = eqnat_ # cn # cm
               in normalOrder (imp_ # premis # conclusion) == tru_
    ]

fac_Tests = testGroup "fac_Tests"
    [ testCase "fac 0 1" $
        normalOrder (fac_ # zro_) @=? one_
    , testCase "fac 1 1" $
        normalOrder (fac_ # one_) @=? one_
    , testCase "fac 2 2" $
        normalOrder (fac_ # n2_) @=? (normalOrder n2_)
   , scProp "fac_ golden" 3 $
       \n -> let cn = SC.getNonNegative n
             in normalOrder (fac_ # (unsafeFromInt cn)) == unsafeFromInt (fac cn)
    ]
  where
    fac x = if x == 0 then 1 else x * fac (pred x)

-- -----------------------------------------------------------------------------
letrecTests = testGroup "Let Tests"
    [ testCase "Let 1 (identity)" $
        let ltc = Let ("i", i_) (Var"i")
        in normalOrder ltc @=? i_
    ]

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- helper functions

unsafeFromInt :: Int -> Exp String
unsafeFromInt = maybe (error "unsafeFromInt") id . fromInt

fromInt :: Int -> Maybe (Exp String)
fromInt i | i < 0       = Nothing
          | otherwise   = Just $ "f" ! "x" ! go i
  where
    go 0 = Var "x"
    go i = Var "f" # go (pred i)


toInt :: Exp String -> Maybe Int
toInt (Lam f (Lam x body)) = go body
    where
       go (Var x) = Just 0
       go (Var f `App` arg) = succ <$> go arg
       go _ = Nothing
toInteger _ = Nothing

unsafeToInt :: Exp String -> Int
unsafeToInt = maybe (error "unsafeToInt") id . toInt

fromBool :: Bool -> Exp String
fromBool True = tru_
fromBool False = fls_

toBool :: Exp String -> Maybe Bool
toBool exp | exp == tru_ = Just True
           | exp == fls_ = Just False
           | otherwise = Nothing

unsafeToBool :: Exp String -> Bool
unsafeToBool = maybe (error "unsafeToBool") id . toBool

fromChar :: Char -> Exp String
fromChar = unsafeFromInt . ord

toChar :: Exp String -> Maybe Char
toChar = fmap chr . toInteger

unsafeToChar :: Exp String -> Char
unsafeToChar = maybe (error "unsafeToChar") id . toChar

-- -----------------------------------------------------------------------------
scProp :: SC.Testable IO a => String -> SC.Depth -> a -> TestTree
scProp s d = SC.testProperty s . SC.changeDepth (const d)

fromNonNegative :: SC.NonNegative Int -> Exp String
fromNonNegative = unsafeFromInt . SC.getNonNegative

fromPositive :: SC.Positive Int -> Exp String
fromPositive = unsafeFromInt . SC.getPositive

interval :: (Enum a, Num a) => a -> a -> QC.Gen a
interval l u = QC.oneof . fmap return $ [l .. u]

