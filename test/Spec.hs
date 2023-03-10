import Control.Monad
import Test.HUnit

main :: IO ()
main = do
  void . runTestTT $
    TestList
      []

{- Test.Parsing.test,
Test.Typing.test,
Test.Refining.Translating.test,
Test.Refining.Check.test -}
