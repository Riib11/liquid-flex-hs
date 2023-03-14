{-# HLINT ignore "Use camelCase" #-}
module Test.Typing where

import Language.Flex.FlexM (runFlexM)
import Language.Flex.Parsing (parseModuleFile)
import Language.Flex.Typing (typeModule)
import System.IO.Unsafe (unsafePerformIO)
import Test.FilePaths (dir_examples, dir_examples_parsing, dir_examples_typing, getDirectoryFilesBySuffix)
import Test.HUnit
import Text.PrettyPrint.HughesPJClass

test :: Test
test =
  TestLabel "typing" $
    TestList
      let !fps =
            unsafePerformIO $
              fmap concat . sequence $
                [getDirectoryFilesBySuffix dir_examples_typing ".flex"]
       in makeTest_procModule <$> fps

makeTest_procModule :: FilePath -> Test
makeTest_procModule fp =
  TestLabel
    ("typing module file: " ++ fp)
    . TestCase
    $ ( do
          putStrLn ""
          mdl <-
            parseModuleFile fp >>= \case
              Left err -> assertFailure (show err)
              Right mdl -> return mdl
          runFlexM (typeModule mdl) >>= \case
            Left err -> assertFailure (render . pPrint $ err)
            Right (_mdl', _env) -> return ()
      )
