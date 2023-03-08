import Control.Monad
import Test.HUnit
import qualified Test.Parsing
import qualified Test.Refining.Check
import qualified Test.Refining.Translating
import qualified Test.Typing

main :: IO ()
main = do
  void . runTestTT $
    TestList
      [ -- Test.Parsing.test,
        -- Test.Typing.test,
        -- Test.Refining.Translating.test,
        Test.Refining.Check.test
      ]
