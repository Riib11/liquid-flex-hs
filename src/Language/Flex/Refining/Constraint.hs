module Language.Flex.Refining.Constraint where

import Control.Lens
import Control.Monad (void)
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Types as F
import Language.Flex.FlexM (FlexM, MonadFlex)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility

-- * Constraint

-- ** Constraint Monoid

data CstrQE = CstrQE
  { cqeQualifiers :: [F.Qualifier],
    cqePred :: [(H.Pred, RefiningError)],
    cqeTermExprs :: [TermExpr]
  }

trivialCstrQE :: CstrQE
trivialCstrQE =
  CstrQE
    { cqeQualifiers = mempty,
      cqePred = mempty,
      cqeTermExprs = mempty
    }

makeSimpleQualifier :: FlexM.MonadFlex m => F.Symbol -> m F.Qualifier
makeSimpleQualifier qName = makeQualifier qName [] (F.prop True)

makeQualifier :: FlexM.MonadFlex m => F.Symbol -> [F.QualParam] -> F.Expr -> m F.Qualifier
makeQualifier qName qParams qBody = do
  qPos <- FlexM.defaultSourcePos
  return
    F.Q
      { qName,
        qParams,
        qBody, -- predicate
        qPos
      }
