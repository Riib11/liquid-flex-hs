module Flex.Refining.Query where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq
import Control.Exception
import Control.Lens
import Control.Monad (foldM, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS.Class (asks, gets)
import Data.Bifunctor (second)
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import Data.Typeable
import Flex.Flex
import Flex.Refining.Check (checkSubtype, checkTerm)
import Flex.Refining.Common
import Flex.Refining.Constraint (Cstr)
import Flex.Refining.Syntax
import Flex.Syntax (Id, Literal, ModuleId)
import qualified Flex.Syntax as Syn
import GHC.Generics
import GHC.IO.Exception (ExitCode)
import qualified Language.Fixpoint.Horn.Solve as HS
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Misc as F
import qualified Language.Fixpoint.Misc as Misc
import qualified Language.Fixpoint.Parse as FP
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.Config as FC
import qualified Language.Fixpoint.Utils.Files as Files
import PrettyShow (PrettyShow (prettyShow))
import System.Exit (exitWith)
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
import Text.Printf (printf)
import Utility

-- | Query
--
-- The query includes all of the constraints gathered up during
-- checking/synthesizing.
type Query = H.Query RefineError

-- | Makes a query that's ready to check the validity of using `checkValid`
makeQuery :: Cstr -> Refining Query
makeQuery cstr = return $ H.Query [] [] cstr mempty mempty mempty mempty mempty

-- Generates a query that checks that the term has the type.
makeCheckTermQuery :: Term -> BaseType -> Refining Query
makeCheckTermQuery tm ty = makeQuery =<< checkTerm tm ty

makeCheckSubtypeQuery :: BaseType -> BaseType -> Refining Query
makeCheckSubtypeQuery ty1 ty2 = makeQuery =<< checkSubtype ty1 ty2

-- | Result
type Result = F.FixResult RefineError

-- resultExitCode :: Result -> IO ExitCode
-- resultExitCode res = do
--   F.colorStrLn (F.colorResult res) (resultString res)
--   case resultErrors res of
--     [] -> return ()
--     errs -> putStrLn . PJ.render =<< checkValid renderRefineErrors errs
--   return (F.resultExit res)

-- resultErrors :: Result -> [RefineError]
-- resultErrors = \case
--   -- TODO: why does each err have a Maybe String also?
--   F.Crash errs msg -> MakeRefineError ("Crash: " <> msg) : (fst <$> errs)
--   F.Unsafe _ errs -> errs
--   F.Safe {} -> []

-- resultString :: Result -> String
-- resultString = \case
--   F.Crash _ msg -> "Crash!: " ++ msg
--   F.Unsafe {} -> "Unsafe"
--   F.Safe {} -> "Safe"

-- | Checks that a query is valid. Throws the appropriate Flex errors if not.
checkValid :: Query -> Refining ()
checkValid q = do
  fp <- gets (^. envFilePath)
  liftIO (checkValidWithConfig fp fpConfig q)
    >>= ( \case
            -- TODO: also include st:Stats in error message?
            F.Crash _ msg -> throwRefiningError $ "Crash: " <> msg
            F.Unsafe _st errs -> throwRefiningError $ "Unsafe: " <> concatMap (("\n" <>) . prettyShow) errs
            F.Safe _st -> return ()
        )

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