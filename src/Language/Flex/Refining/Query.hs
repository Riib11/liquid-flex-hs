module Language.Flex.Refining.Query where

import Control.Monad.Reader (MonadIO (liftIO), MonadTrans (lift), asks, when)
import qualified Language.Fixpoint.Horn.Solve as HS
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Misc as Misc
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.Config as FC
import qualified Language.Fixpoint.Utils.Files as Files
import Language.Flex.FlexM (FlexOptions (FlexOptions, sourceFilePath))
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Types
import qualified Text.PrettyPrint.HughesPJ as PJ

makeQuery :: Cstr -> RefiningM Query
makeQuery cstr = return $ H.Query [] [] cstr mempty mempty mempty mempty mempty

-- | Submit query to LH backend, which checks for validity
submitQuery :: Query -> RefiningM Result
submitQuery q = do
  fp <- liftFlexM . asks $ sourceFilePath
  liftIO (checkValidWithConfig fp fpConfig q)
    >>= error "TODO: handle result"

fpConfig :: FC.Config
fpConfig =
  FC.defConfig
    { FC.eliminate = FC.Some
    }

checkValidWithConfig :: FilePath -> FC.Config -> Query -> IO Result
checkValidWithConfig fp config query = do
  dumpQuery fp query
  fmap snd . F.resStatus <$> HS.solve config query

dumpQuery :: FilePath -> Query -> IO ()
dumpQuery fp query = when True do
  let smtFile = Files.extFileName Files.Smt2 fp
  Misc.ensurePath smtFile
  writeFile smtFile (PJ.render . F.pprint $ query)