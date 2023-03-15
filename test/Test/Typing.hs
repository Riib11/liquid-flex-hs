{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use ++" #-}
module Test.Typing where

import Control.Monad (unless, when)
import Language.Flex.DefaultFlexOptions (defaultFlexOptions)
import Language.Flex.FlexM (FlexOptions (flexVerbose), runFlexM)
import Language.Flex.Parsing (parseModuleFile)
import Language.Flex.Typing (typeModule)
import System.IO.Unsafe (unsafePerformIO)
import Test.FilePaths (dir_examples, dir_examples_parsing, dir_examples_typing, dir_examples_typing_fail, getDirectoryFilesBySuffix)
import Test.HUnit
import Text.PrettyPrint.HughesPJClass hiding ((<>))

test :: Test
test =
  TestLabel "typing" $
    TestList $
      concat
        [ let !fps =
                unsafePerformIO $
                  fmap concat . sequence $
                    [getDirectoryFilesBySuffix dir_examples_typing ".flex"]
           in makeTest_procModule True <$> fps,
          let !fps =
                unsafePerformIO $
                  fmap concat . sequence $
                    [getDirectoryFilesBySuffix dir_examples_typing_fail ".flex"]
           in makeTest_procModule False <$> fps
        ]

makeTest_procModule :: Bool -> FilePath -> Test
makeTest_procModule pass fp =
  TestLabel
    ("typing module file: " ++ fp)
    . TestCase
    $ do
      putStrLn ""
      mdl <-
        parseModuleFile fp >>= \case
          Left err -> assertFailure (show err)
          Right mdl -> return mdl
      runFlexM defaultFlexOptions (typeModule mdl) >>= \case
        Left err -> when pass $ assertFailure (render . pPrint $ err)
        Right (_mdl', _env) -> do unless pass $ assertFailure "expected typing to fail"
