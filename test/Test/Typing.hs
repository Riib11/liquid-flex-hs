module Test.Typing where

import Test.HUnit
import Text.PrettyPrint.HughesPJClass

test :: Test
test =
  TestLabel "typing" $
    TestList
      [ test_synthCheckTerm
      ]

-- TODO: some tests
test_synthCheckTerm :: Test
test_synthCheckTerm =
  TestLabel "synthCheckTerm" $
    TestList
      []
