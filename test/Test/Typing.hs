{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use ++" #-}
module Test.Typing where

import Control.Monad (unless, when)
import Control.Monad.Except (runExceptT)
import Language.Flex.DefaultFlexCtx (defaultFlexCtx)
import Language.Flex.Elaboration (elaborateModule)
import Language.Flex.FlexM (FlexCtx (flexDebug, flexVerbose), runFlexM)
import Language.Flex.Parsing (parseModuleFile)
import Language.Flex.Typing (typeModule)
import System.IO.Unsafe (unsafePerformIO)
import Test.FilePaths (dir_examples, dir_examples_parsing, dir_examples_refining, dir_examples_refining_fail, dir_examples_typing, dir_examples_typing_fail, getDirectoryFilesBySuffix)
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
           in makeTest_procModule False <$> fps,
          let !fps =
                unsafePerformIO $
                  fmap concat . sequence $
                    [getDirectoryFilesBySuffix dir_examples_refining ".flex"]
           in makeTest_procModule True <$> fps,
          let !fps =
                unsafePerformIO $
                  fmap concat . sequence $
                    [getDirectoryFilesBySuffix dir_examples_refining_fail ".flex"]
           in makeTest_procModule True <$> fps
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
      mdl' <- runFlexM (defaultFlexCtx {flexDebug = False}) do
        elaborateModule mdl
      runFlexM (defaultFlexCtx {flexDebug = False}) (runExceptT $ typeModule mdl') >>= \case
        Left err -> when pass $ assertFailure (render . pPrint $ err)
        Right (_mdl', _env) -> do unless pass $ assertFailure "expected typing to fail"
