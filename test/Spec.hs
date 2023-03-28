import Control.Monad
import Test.HUnit
import qualified Test.Parsing as Parsing
import qualified Test.Refining as Refining
import qualified Test.Typing as Typing

main :: IO ()
main = do
  void . runTestTT $
    TestList
      [ -- Parsing.test,
        -- Typing.test,
        Refining.test
      ]
