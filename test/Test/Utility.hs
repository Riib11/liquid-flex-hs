module Test.Utility where

import PrettyShow
import Test.HUnit

assertEqualPretty :: (Eq a, PrettyShow a) => a -> a -> Assertion
assertEqualPretty aInf aExp
  | aInf == aExp = return ()
  | otherwise =
    assertFailure $
      unlines
        [ "the tested result is:",
          indent (prettyShow aInf),
          "but the expected result is:",
          indent (prettyShow aExp)
        ]
