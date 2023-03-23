{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use camelCase" #-}
module Test.Refining where

import Control.Monad
import Data.List (singleton)
import Language.Flex.DefaultFlexCtx (defaultFlexCtx)
import Language.Flex.FlexM (FlexCtx (flexVerbose), runFlexM)
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
      -- [ let !fps =
      --         unsafePerformIO $
      --           fmap concat . sequence $
      --             [getDirectoryFilesBySuffix dir_examples_refining ".flex"]
      --    in makeTest_refineModule True <$> fps,
      --   let !fps =
      --         unsafePerformIO $
      --           fmap concat . sequence $
      --             [getDirectoryFilesBySuffix dir_examples_refining_fail ".flex"]
      --    in makeTest_refineModule False <$> fps
      -- ]
      $
        singleton $
          [makeTest_refineModule True "examples/refining/Structures.flex"]

makeTest_refineModule :: Bool -> FilePath -> Test
makeTest_refineModule pass fp = TestLabel ("refining module file: " ++ fp) . TestCase $ do
  putStrLn ""

  !_ <- return ()

  mdl <-
    parseModuleFile fp >>= \case
      Left err -> assertFailure (render $ "parsing failure in refinement test:" $$ text (show err))
      Right mdl -> return mdl

  !_ <- return ()

  putStrLn "[parsed module]"
  putStrLn $ replicate 40 '='
  putStrLn $ render (pPrint mdl)
  putStrLn $ replicate 40 '='

  !_ <- return ()

  mdl' <-
    runFlexM defaultFlexCtx (typeModule mdl) >>= \case
      Left err -> assertFailure (render $ "[error: typing failure in refinement test]" $$ pPrint err)
      Right (mdl', _env) -> return mdl' -- assertFailure "typing failed in refining test"
  !_ <- return ()

  putStrLn "[typed module]"
  putStrLn $ replicate 40 '='
  putStrLn $ render (pPrint mdl')
  putStrLn $ replicate 40 '='

  !_ <- return ()

  runFlexM defaultFlexCtx {flexVerbose = True} (refineModule mdl') >>= \case
    Left err -> when pass $ assertFailure (render . pPrint $ err)
    Right _ -> unless pass $ assertFailure "expected refining to fail"
