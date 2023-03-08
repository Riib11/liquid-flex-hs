{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Missing NOINLINE pragma" #-}
module Test.Refining.Translating (test) where

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
import Flex.Shallow
import Flex.Syntax (mapTermType)
import qualified Flex.Syntax as Base
import Flex.Typing (checkTerm, defaultType, runTyping)
import qualified Flex.Typing as Typing
import PrettyShow
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit hiding (test)
import Test.Utility (assertEqualPretty)
import Text.Parsec (runParserT)
import Utility

test :: Test
test =
  TestLabel "Refining.Translating" $
    TestList
      [ test_transLiteral,
        test_transApp
      ]

test_transLiteral :: Test
test_transLiteral =
  TestLabel "transLiteral" $
    TestList . map TestCase $
      [ makeTest_transTerm
          defaultOptions
          "true"
          Nothing,
        makeTest_transTerm
          defaultOptions
          "false"
          Nothing,
        makeTest_transTerm
          defaultOptions
          "1"
          Nothing,
        makeTest_transTerm
          defaultOptions
          "-1"
          Nothing,
        makeTest_transTerm
          defaultOptions
          "'c'"
          Nothing,
        -- TODO: transType TypeArray (for string type)
        -- makeTest_transTerm
        --   defaultOptions
        --   "\"s\""
        --   Nothing,
        makeTest_transTerm
          defaultOptions
          "true"
          (Just $ Term (TermLiteral (Base.LiteralBit True)) (bitBaseType mempty)),
        makeTest_transTerm
          defaultOptions {pass = False}
          "false"
          (Just $ Term (TermLiteral (Base.LiteralBit True)) (bitBaseType mempty))
      ]

test_transApp :: Test
test_transApp =
  TestLabel "transApp" $
    TestList . map TestCase $
      [ makeTest_transTerm
          defaultOptions {mb_module = Just mdl}
          "f0()"
          (Just $ Term (TermApp (AppVar "f0#4") []) (bitBaseType mempty)),
        makeTest_transTerm
          defaultOptions {mb_module = Just mdl}
          "f1(true)"
          Nothing,
        makeTest_transTerm
          defaultOptions {mb_module = Just mdl}
          "f1(true)"
          (Just $ Term (TermApp (AppVar "f1#5") [Term (TermLiteral (Base.LiteralBit True)) (bitBaseType mempty)]) (bitBaseType mempty))
      ]
  where
    mdl :: Base.Module
    mdl = unsafePerformIO $ Examples.module_Simple

data Options = Options
  { pass :: Bool,
    verbose :: Bool,
    mb_module :: Maybe Base.Module
  }

defaultOptions :: Options
defaultOptions =
  Options
    { pass = True,
      verbose = False,
      mb_module = Nothing
    }

-- be careful with asserting equality, because logically equivalent refinements
-- may not always be syntactically equivalent
makeTest_transTerm :: Options -> String -> Maybe Term -> IO ()
makeTest_transTerm opts tmStr mb_tm = do
  tmParsed <- readTerm tmStr
  ( runFlexM topFlexEnv do
      forM_ (mb_module opts) loadModule
      tmTyped <-
        mapTermType defaultType
          <$> runTyping
            Typing.emptyCtx
            ( do
                -- if a module was specified, need to typecheck it first
                forM_ (mb_module opts) Typing.procModule
                Typing.inferTerm tmParsed
            )
      let ptm = tmTyped ^. Base.termPreterm
          ty =
            fromJustDefault (error "type-checked term must have annotated type") $
              tmTyped ^. Base.termMaybeType
      verboseLog opts $ "base term   = " <> prettyShow ptm <> " : " <> prettyShow ty
      runRefining $ transTerm tmTyped
    )
    >>= \case
      Left err -> when (pass opts) do
        assertFailure ("Flex error: " <> show err)
      Right (tm', _) -> do
        verboseLog opts $ "liquid term = " <> prettyShow (termPreterm tm') <> " : " <> prettyShow (termType tm')
        when (pass opts) case mb_tm of
          Nothing -> return ()
          Just tm -> do
            -- test separately for sake of pretty output, since prettyShow on a Term
            -- doesn't show the refinement type
            assertEqualPretty (termPreterm tm') (termPreterm tm)
            assertEqualPretty (termType tm') (termType tm)

-- verboseLog :: Options -> String -> IO ()
verboseLog :: MonadIO f => Options -> String -> f ()
verboseLog opts str =
  when (verbose opts) . liftIO . putStrLn $
    "\n[Test/Refining] " <> str
