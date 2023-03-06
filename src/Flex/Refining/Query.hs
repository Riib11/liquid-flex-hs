module Flex.Refining.Query where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq
import Control.Exception
import Control.Monad (foldM, void, when)
import Data.Bifunctor (second)
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import Data.Typeable
import Flex.Refining.CG
import Flex.Refining.Check (check)
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
import System.Exit (exitWith)
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
import Text.Printf (printf)
import Utility

-- | Query
--
-- The query includes all of the constraints gathered up during
-- checking/synthesizing.
type Query = H.Query RefineError

-- Generates a query that checks that the term has the type.
genCheckQuery :: Env -> Term -> BaseType -> CG Query
genCheckQuery env tm ty =
  H.Query [] []
    <$> check env tm ty
    <*> pure mempty
    <*> pure mempty
    <*> pure mempty
    <*> pure mempty
    <*> pure mempty

-- | Result
type Result = F.FixResult RefineError

resultExitCode :: Result -> IO ExitCode
resultExitCode res = do
  F.colorStrLn (F.colorResult res) (resultString res)
  case resultErrors res of
    [] -> return ()
    errs -> putStrLn . PJ.render =<< renderRefineErrors errs
  return (F.resultExit res)

resultErrors :: Result -> [RefineError]
resultErrors = \case
  -- TODO: why does each err have a Maybe String also?
  F.Crash errs msg -> RefineError ("Crash: " <> msg) : (fst <$> errs)
  F.Unsafe _ errs -> errs
  F.Safe {} -> []

resultString :: Result -> String
resultString = \case
  F.Crash _ msg -> "Crash!: " ++ msg
  F.Unsafe {} -> "Unsafe"
  F.Safe {} -> "Safe"

-- | checkValid
checkValid :: FilePath -> Query -> IO Result
checkValid fp = checkValidWithConfig fp fpConfig

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