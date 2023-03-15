{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use camelCase" #-}
module Test.Refining where

import Control.Monad
import Language.Flex.DefaultFlexOptions (defaultFlexOptions)
import Language.Flex.FlexM (FlexOptions (flexVerbose), runFlexM)
import Language.Flex.Parsing (parseModuleFile)
import Language.Flex.Refining (refineModule)
import Language.Flex.Typing (typeModule)
import System.IO.Unsafe (unsafePerformIO)
import Test.FilePaths
import Test.HUnit
import Text.PrettyPrint.HughesPJ (render, text, ($$), (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))

test :: Test
test =
  TestLabel "refining" $
    TestList $
      concat
        [ let !fps =
                unsafePerformIO $
                  fmap concat . sequence $
                    [getDirectoryFilesBySuffix dir_examples_refining ".flex"]
           in makeTest_refineModule True <$> fps,
          let !fps =
                unsafePerformIO $
                  fmap concat . sequence $
                    [getDirectoryFilesBySuffix dir_examples_refining_fail ".flex"]
           in makeTest_refineModule False <$> fps
        ]

makeTest_refineModule :: Bool -> FilePath -> Test
makeTest_refineModule pass fp = TestLabel ("refining module file: " ++ fp) . TestCase $ do
  putStrLn ""
  mdl <-
    parseModuleFile fp >>= \case
      Left err -> assertFailure (render $ "parsing failure in refinement test:" $$ text (show err))
      Right mdl -> return mdl
  mdl' <-
    runFlexM defaultFlexOptions (typeModule mdl) >>= \case
      Left err -> assertFailure (render $ "typing failure in refinement test:" <+> pPrint err)
      Right (mdl', _env) -> return mdl' -- assertFailure "typing failed in refining test"
  runFlexM defaultFlexOptions {flexVerbose = True} (refineModule mdl') >>= \case
    Left err -> when pass $ assertFailure (render . pPrint $ err)
    Right _ -> unless pass $ assertFailure "expected refining to fail"
