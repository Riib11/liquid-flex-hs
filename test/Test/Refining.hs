{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Test.Refining (test) where

import Control.Monad
import Control.Monad.Reader (MonadIO (liftIO), MonadTrans (lift), runReaderT)
import Flex.Flex
import Flex.Lexing (Parser)
import qualified Flex.Lexing as Parsing
import Flex.Parsing
import Flex.Refining
import qualified Flex.Refining.Syntax as Base
import Flex.Syntax (mapTermType)
import qualified Flex.Syntax as Base
import Flex.Typing (checkTerm, defaultType, runTyping)
import qualified Flex.Typing as Typing
import PrettyShow
import Test.HUnit hiding (test)
import Test.Utility (assertEqualPretty)
import Text.Parsec (runParserT)
import Utility

test :: Test
test =
  TestLabel "Refining" $
    TestList
      [ test_transLiteral
      ]

test_transLiteral :: Test
test_transLiteral =
  TestLabel "transLiteral" $
    TestList
      [ TestCase $
          (makeTest_transTerm True)
            "true"
            (Term (TermLiteral (Base.LiteralBit True)) (bitBaseType mempty)),
        TestCase $
          (makeTest_transTerm False)
            "false"
            (Term (TermLiteral (Base.LiteralBit True)) (bitBaseType mempty))
      ]

makeTest_transTerm :: Bool -> String -> Term -> IO ()
makeTest_transTerm pass tmString tm = do
  tmParsed <- runParser "makeTest_transTerm" parseTerm tmString
  ( runFlexT topFlexEnv do
      tmTyped <-
        mapTermType defaultType
          <$> runTyping Typing.emptyCtx (Typing.inferTerm tmParsed)
      runTranslating $ transTerm tmTyped
    )
    >>= \case
      Left err -> when pass do
        assertFailure ("Flex error: " <> show err)
      Right (tm', _) -> when pass do
        assertEqualPretty tm' tm

runParser :: String -> Parser a -> String -> IO a
runParser label parser string =
  runParserT
    parser
    (Parsing.emptyEnv Base.topModuleId)
    ("Test(" <> label <> ")")
    string
    >>= \case
      Left err -> error $ "parse error: " <> show err
      Right a -> return a
