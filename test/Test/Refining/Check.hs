{-# HLINT ignore "Use camelCase" #-}
module Test.Refining.Check where

import Control.Lens
import Control.Monad
import Control.Monad.Reader (MonadIO (liftIO), MonadTrans (lift), runReaderT)
import Data.Maybe (fromJust)
import qualified Flex.Examples as Examples
import Flex.Flex
import Flex.Lexing (Parser)
import qualified Flex.Lexing as Parsing
import Flex.Parsing
import Flex.Refining
import qualified Flex.Refining as Refining
import Flex.Shallow
import Flex.Syntax (mapTermType)
import qualified Flex.Syntax as Base
import Flex.Typing (checkTerm, defaultType, runTyping)
import qualified Flex.Typing as Typing
import qualified Language.Fixpoint.Types as F
import PrettyShow
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit hiding (test)
import Test.Utility (assertEqualPretty)
import Text.Parsec (runParserT)
import Utility

test :: Test
test =
  TestLabel "Refining.Check" $
    TestList
      [ -- test_tmp
        test_checkTerm
      ]

{- TMP example for testing subtype checking
test_tmp :: Test
test_tmp =
  TestLabel "tmp" . TestCase $ do
    res <- runFlexM topFlexEnv do
      Refining.runRefining do
        -- Refining.checkValid
        Refining.checkValid
          =<< Refining.makeCheckSubtypeQuery
            (TypeAtomic F.trueReft AtomicBit)
            (TypeAtomic F.falseReft AtomicBit)
    case res of
      Left fe -> assertFailure $ "refinement-check failure: " <> prettyShow fe
      Right (a, _) -> assertFailure $ "refinement-check success"
-}

test_checkTerm :: Test
test_checkTerm =
  TestLabel "checkTerm" $
    TestList . fmap TestCase $
      [ makeTest_checkTerm True "true" "bit",
        makeTest_checkTerm True "true || false" "bit",
        makeTest_checkTerm True "1 + 1" "int32",
        makeTest_checkTerm False "1 + true" "int32",
        makeTest_checkTerm False "1 / 0" "int32",
        makeTest_checkTerm True "1 / 1" "int32"
      ]

makeTest_checkTerm :: Bool -> String -> String -> IO ()
makeTest_checkTerm pass tmStr tyStr = do
  -- parse
  tmBase <- readTerm tmStr
  tyBase <- readType tyStr
  res <- runFlexM topFlexEnv do
    -- typecheck
    tmTyped <- Typing.runTyping Typing.emptyCtx do
      tm <- Typing.checkInferTerm tmBase tyBase
      return $ Base.mapTermMaybeType (Typing.defaultType <$>) tm
    tyTyped <- Typing.runTyping Typing.emptyCtx do
      Typing.defaultType <$> Typing.normType tyBase
    Refining.runRefining do
      -- translate
      tm <- Refining.transTerm tmTyped
      ty <- Refining.transType tyTyped
      -- refinecheck
      Refining.checkValid =<< Refining.makeCheckTermQuery tm ty
      return (tm, ty)
  case res of
    Left fe -> when pass do
      assertFailure $
        unlines . fmap ("    " <>) $
          [ "expected refinement check",
            "    " <> prettyShow tmBase <> " : " <> prettyShow tyBase,
            "to pass, but instead got Flex error:",
            "    " <> prettyShow fe
          ]
    Right ((tm, ty), _) -> unless pass do
      assertFailure $
        unlines . fmap ("    " <>) $
          [ "expected refinement check",
            "    " <> prettyShow tm <> " : " <> prettyShow ty,
            "to fail, but instead got no refinement error."
          ]