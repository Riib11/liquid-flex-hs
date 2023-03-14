{-# HLINT ignore "Use camelCase" #-}
module Test.Typing where

import Language.Flex.FlexM (runFlexM)
import Language.Flex.Parsing (parseModuleFile)
import Language.Flex.Typing (typeModule)
import Test.HUnit
import Text.PrettyPrint.HughesPJClass

test :: Test
test =
  TestLabel "typing" $
    TestList
      []

makeTest_procModule :: FilePath -> Test
makeTest_procModule fp =
  TestLabel
    ("typing module file: " ++ fp)
    . TestCase
    $ ( do
          mdl <-
            parseModuleFile fp >>= \case
              Left err -> assertFailure (show err)
              Right mdl -> return mdl
          runFlexM (typeModule mdl) >>= \case
            Left err -> assertFailure (render . pPrint $ err)
            Right (_mdl', _env) -> return ()
      )
