module Test.Utility where

import Control.Monad (unless)
import Test.HUnit (Assertion, assertFailure)
import Text.PrettyPrint.HughesPJClass

assertEqualPretty :: (Eq a, Pretty a) => a -> a -> Assertion
assertEqualPretty = assertRelationPretty "equal" (==)

assertRelationPretty :: Pretty a => String -> (a -> a -> Bool) -> a -> a -> Assertion
assertRelationPretty label rel expect synth =
  unless (rel expect synth) do
    assertFailure . render $
      "the actual result is"
        $$ nest 4 (pPrint synth)
        $$ "which is not"
        <+> text label
        <+> "to the expected result:"
        $$ nest 4 (pPrint expect)
