{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Language.Flex.Refining.Query where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Foldable
import qualified Data.Map as Map
import qualified Language.Fixpoint.Horn.Solve as HS
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Misc as Misc
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.Config as FC
import qualified Language.Fixpoint.Utils.Files as Files
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Primitive (primitiveDataDecls)
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Reflecting
import Language.Flex.Refining.Syntax
import Language.Flex.Refining.Translating
import qualified Language.Flex.Syntax as Base
import Text.PrettyPrint.HughesPJ hiding (first, (<>))

-- ** Submitting Query

-- | Submit query to LH backend, which checks for validity
submitQuery :: Query -> RefiningM Result
submitQuery q = do
  fp <- FlexM.liftFlex . asks $ FlexM.flexSourceFilePath
  liftIO (checkValidWithConfig fp fpConfig q)

fpConfig :: FC.Config
fpConfig =
  FC.defConfig
    { FC.eliminate = FC.Some,
      FC.oldPLE = True
    }

checkValidWithConfig :: FilePath -> FC.Config -> Query -> IO Result
checkValidWithConfig fp config query = do
  dumpQuery fp query
  fmap snd . F.resStatus <$> HS.solve config query

-- !TODO what does this do exactly??
dumpQuery :: FilePath -> Query -> IO ()
dumpQuery fp query = when True do
  let smtFile = Files.extFileName Files.Smt2 fp
  Misc.ensurePath smtFile
  writeFile smtFile (render . F.pprint $ query)
