{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use ++" #-}

module Test.Typing where

import Control.Monad
import Flex.Flex
import Flex.Lexing (Parser)
import qualified Flex.Lexing as Parsing
import Flex.Parsing
import Flex.Shallow (readTerm, readType)
import Flex.Syntax
import Flex.Typing
import PrettyShow
import Test.HUnit
import Text.Parsec (runParserT)
import Utility

test :: Test
test =
  TestLabel "Typing" $
    TestList
      [ test_checkTerm,
        test_checkModule
      ]

-- [ test_checkTerm_array
-- ]

test_checkModule :: Test
test_checkModule =
  TestLabel "checkModule" $
    TestList
      [ test_checkModule_1
      ]

test_checkModule_1 :: Test
test_checkModule_1 =
  TestLabel "checkModule_1" . TestList $
    []

--
-- makeTest_checkModule $ mdTop \mdId ->
--   (dcStructure "A" True Nothing)
--     [("x", tyInt 32), ("y", tyInt 32)]
--     Nothing
--     \a -> [],
-- --
-- makeTest_checkModule $ mdTop \mdId ->
--   (dcStructure "B" True Nothing)
--     [("content", tyString)]
--     Nothing
--     \b ->
--       [],
-- --
-- makeTest_checkModule $ mdTop \mdId ->
--   -- TODO: try turning this into a newtype
--   (dcStructure "radians" True Nothing)
--     [("radians_value", tyFloat64)]
--     Nothing
--     \radians ->
--       (dcFunction "validate" False [("p", tyStruct radians), ("q", tyStruct radians)] tyBit)
--         ( tmEq
--             (tmMem radians (tmLocal "p") "radians_value")
--             -- (tmLocal "p")
--             (tmMem radians (tmLocal "q") "radians_value")
--         )
--         \validate -> []

test_checkTerm :: Test
test_checkTerm =
  TestLabel "checkTerm" $
    TestList
      [ test_checkTerm_lit,
        test_checkTerm_tuple,
        test_checkTerm_array
      ]

test_checkTerm_lit :: Test
test_checkTerm_lit =
  TestLabel "checkTerm lit" . TestList $
    makeTests_checkTerm
      -- passes
      [ ("10", "int 32"),
        ("10", "int 64"),
        ("-10", "int 64"),
        ("-10", "int 64")
      ]
      -- fails
      [ ("3.14", "float 32"),
        ("3.14", "float 64")
      ]

test_checkTerm_tuple :: Test
test_checkTerm_tuple =
  TestLabel "checkTerm tuple" . TestList $
    makeTests_checkTerm
      -- passes
      [ ("(1, 2, 3)", "(int 32, int 32, int 32)"),
        ("('a', 2)", "(char, int 32)"),
        ("('a', ('b', ('c', 'd')))", "(char, (char, (char, char)))")
      ]
      -- fails
      []

test_checkTerm_array :: Test
test_checkTerm_array =
  TestLabel "checkTerm array" . TestList $
    makeTests_checkTerm
      -- passes
      [ ("[1, 2, 3]", "[int 32]"),
        ("[1, -2, 3]", "[int 32]")
      ]
      -- fails
      [ ("[1, -2, 3]", "[char]"),
        ("[1, -2, 'c']", "[int 32]")
      ]

-- * utilities

makeTests_checkTerm :: [(String, String)] -> [(String, String)] -> [Test]
makeTests_checkTerm passes fails =
  concat
    [ uncurry (makeTest_checkTerm True) <$> passes,
      uncurry (makeTest_checkTerm False) <$> fails
    ]

makeTest_checkTerm :: Bool -> String -> String -> Test
makeTest_checkTerm pass tmStr tyStr = TestCase do
  tm <- readTerm tmStr
  ty <- readType tyStr
  ei_tm <- runFlexT topFlexEnv $ runTyping emptyCtx $ liftM2' checkTerm (inferTerm tm) (return ty)

  case ei_tm of
    Left err ->
      when pass $
        assertFailure $
          unlines . fmap ("    " <>) $
            [ "expected",
              "    " <> prettyShow tm <> " : " <> prettyShow ty,
              "to pass, but instead got a type error:",
              show err
            ]
    Right _ ->
      unless pass $
        assertFailure $
          unlines . fmap ("    " <>) $
            [ "expected",
              "    " <> prettyShow tm <> " : " <> prettyShow ty,
              "to fail, but instead got no typing error."
            ]

-- makeTest_checkModule :: Module -> Test
-- makeTest_checkModule mdl = TestCase do
--   (_, errs) <- runTyping $ checkModule mdl
--   unless (null errs) do
--     assertFailure $
--       unlines . fmap ("    " <>) $
--         [ "failed to typecheck module:",
--           unlines (bullet . show <$> errs)
--         ]

bullet :: String -> String
bullet str = case lines str of
  [] -> ""
  [x] -> "  • " <> x
  (x : xs) -> " • " <> x <> "\n" <> (unlines . fmap ("    " <>) $ xs)
