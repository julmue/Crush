import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Language.Lambda.Semantics.Named.BigStep.Tests
import Language.Lambda.Semantics.Named.SmallStep.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ bigStepTests
    , smallStepTests
    ]
