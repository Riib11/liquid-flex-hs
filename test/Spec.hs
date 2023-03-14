import Control.Monad
import Test.HUnit
import qualified Test.Parsing as Parsing

main :: IO ()
main = do
  void . runTestTT $
    TestList
      [Parsing.test]

{- Test.Parsing.test,
Test.Typing.test,
Test.Refining.Translating.test,
Test.Refining.Check.test -}
