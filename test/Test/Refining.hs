{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use camelCase" #-}
module Test.Refining where

import Control.Monad
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.List (singleton)
import Language.Flex.DefaultFlexCtx (defaultFlexCtx)
import Language.Flex.FlexM (FlexCtx (flexDebug), FlexM, runFlexM)
import Language.Flex.Parsing (parseModuleFile)
import Language.Flex.Refining (refineModule)
import Language.Flex.Typing (typeModule)
import Language.Flex.Typing.TypingM (TypingError (TypingError))
import System.IO.Unsafe (unsafePerformIO)
import Test.FilePaths
import Test.HUnit
import Text.PrettyPrint.HughesPJ (render, text, ($$), (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))

-- whether to do FULL test suite or just the specified tests
_FULL = True

_DEBUG = not _FULL

test :: Test
test =
  TestLabel "refining" $
    TestList $
      concat
        if _FULL
          then
            [ let !fps =
                    unsafePerformIO . fmap concat . sequence $
                      [getDirectoryFilesBySuffix dir_examples_refining ".flex"]
               in makeTest_refineModule True <$> fps,
              let !fps =
                    unsafePerformIO . fmap concat . sequence $
                      [getDirectoryFilesBySuffix dir_examples_refining_fail ".flex"]
               in makeTest_refineModule False <$> fps
            ]
          else
            [ [ -- makeTest_refineModule True "examples/refining/Tuples.flex",
                -- makeTest_refineModule True "examples/refining/Locals.flex",
                -- makeTest_refineModule True "examples/refining/Parameters.flex",
                -- makeTest_refineModule True "examples/refining/StructureConstructors.flex",
                makeTest_refineModule True "examples/refining/Structures.flex" -- !TODO get introducing refined structures to work
                -- makeTest_refineModule True "examples/refining/Functions.flex"
                -- makeTest_refineModule True "examples/refining/Matches.flex"
              ]
              --  [makeTest_refineModule True "examples/refining/Structures.flex"]
              -- [makeTest_refineModule True "examples/refining/Variants.flex"]
            ]

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
    (runFlexM defaultFlexCtx . runExceptT) (typeModule mdl) >>= \case
      Left err -> assertFailure (render $ "[error: typing failure in refinement test]" $$ pPrint err)
      Right (_env, mdl') -> return mdl'
  !_ <- return ()

  putStrLn "[typed module]"
  putStrLn $ replicate 40 '='
  putStrLn $ render (pPrint mdl')
  putStrLn $ replicate 40 '='

  !_ <- return ()

  (runFlexM defaultFlexCtx {flexDebug = _DEBUG} . runExceptT) (refineModule mdl') >>= \case
    Left err -> when pass $ assertFailure (render . pPrint $ err)
    Right _ -> unless pass $ assertFailure "expected refining to fail"
