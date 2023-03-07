module Flex.Refining.Refining where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq
import Control.Exception
import Control.Monad (foldM, void, when)
import Data.Bifunctor (second)
-- import Flex.Refining.Syntax

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import Data.Typeable
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

-- | Constraint Generation monad
type Refining a = Either [RefineError] a

throwRefining :: [RefineError] -> Refining a
throwRefining = Left

unsafeRefining :: Refining a -> a
unsafeRefining cg = case cg of
  Left errs -> error $ "unsafeRefining: \n" <> List.intercalate "\n" (show <$> errs)
  Right a -> a

-- | RefineError
newtype RefineError = RefineError String
  deriving (Generic, Show)

instance NFData RefineError

instance Exception [RefineError]

messageOfRefineError :: RefineError -> String
messageOfRefineError (RefineError msg) = msg

labelOfRefineError :: RefineError -> Label
labelOfRefineError _ = F.dummySpan

instance F.PPrint RefineError where
  pprintTidy k = F.pprintTidy k . refineErrorFP

instance F.Fixpoint RefineError where
  toFix = PJ.text . messageOfRefineError

instance F.Loc RefineError where
  srcSpan = labelOfRefineError

fpRefineError :: F.Error1 -> RefineError
fpRefineError e = RefineError (show $ F.errMsg e)

refineErrorFP :: RefineError -> F.Error
refineErrorFP err =
  F.err
    (labelOfRefineError err)
    (PJ.text $ messageOfRefineError err)

renderRefineError :: RefineError -> IO PJ.Doc
renderRefineError (RefineError msg) = do
  -- TODO: can also look up snippet where error originated
  return $ PJ.text msg

renderRefineErrors :: [RefineError] -> IO PJ.Doc
renderRefineErrors errs = do
  errs' <- mapM renderRefineError errs
  return $ PJ.vcat (PJ.text "Errors found!" : PJ.text "" : errs')

-- | Label
type Label = F.SrcSpan

class HasLabel a where
  getLabel :: a -> Label
