module Test.Utility where

import PrettyShow
import Test.HUnit

assertEqualPretty :: (Eq a, PrettyShow a) => a -> a -> Assertion
assertEqualPretty aInf aExp
  | aInf == aExp = return ()
  | otherwise =
      assertFailure $
        unlines
          [ "the tested result is:\n",
            indent (prettyShow aInf),
            "\nbut the expected result is:\n",
            indent (prettyShow aExp)
          ]
