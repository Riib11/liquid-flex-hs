{-# HLINT ignore "Use camelCase" #-}
module Test.Parsing where

import Control.Monad
import Language.Flex.Parsing (parseModuleFile)
import System.IO.Unsafe (unsafePerformIO)
import Test.FilePaths (dir_examples, dir_examples_parsing, dir_examples_typing, getDirectoryFilesBySuffix)
import Test.HUnit
import Text.PrettyPrint.HughesPJ (nest, render, text, vcat, ($$))
import Utility

test :: Test
test =
  TestLabel "parsing" $
    TestList
      [test_parseModule]

test_parseModule :: Test
test_parseModule =
  let !fps =
        unsafePerformIO $
          fmap concat . sequence $
            [ getDirectoryFilesBySuffix dir_examples_parsing ".flex",
              getDirectoryFilesBySuffix dir_examples_typing ".flex"
            ]
   in TestLabel "parseModule" $
        TestList $
          makeTest_parseModule <$> fps

makeTest_parseModule :: FilePath -> Test
makeTest_parseModule fp =
  TestLabel
    ("parsing module file: " <> fp)
    . TestCase
    $ ( do
          putStrLn ""
          parseModuleFile fp >>= \case
            Left err -> assertFailure (show err)
            Right _ -> return ()
      )