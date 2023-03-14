{-# HLINT ignore "Use newtype instead of data" #-}
module Language.Flex.Refining.RefiningM where

-- import Language.Fixpoint.Types.PrettyPrint

import Control.DeepSeq (NFData)
import Control.Monad.Except (ExceptT, MonadTrans (lift))
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.State (StateT)
import qualified Data.Map as Map
import GHC.Generics
import qualified Language.Fixpoint.Types as F
import Language.Fixpoint.Types.PrettyPrint (pprint)
import qualified Language.Flex.FlexBug as FlexBug
import Language.Flex.FlexM (FlexLog (FlexLog), FlexM)
import Language.Flex.Refining.Syntax
import Language.Flex.Syntax (Module, TermId, TypeId)
import Text.PrettyPrint.HughesPJ (Doc, nest, render, text, ($$), (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))

-- ** RefiningM

type RefiningM = StateT RefiningEnv (ReaderT RefiningCtx (ExceptT RefiningError FlexM))

liftFlexM :: FlexM a -> RefiningM a
liftFlexM = lift . lift . lift

newtype RefiningError = RefiningError Doc
  deriving (Generic, Show)

instance NFData RefiningError

instance Pretty RefiningError where
  pPrint (RefiningError msg) = "refining error:" <+> msg

instance F.Fixpoint RefiningError where
  toFix = pPrint

instance F.Loc RefiningError where
  srcSpan _ = F.dummySpan

instance F.PPrint RefiningError where
  pprintTidy _ = text . render . pPrint

data RefiningCtx = RefiningCtx {}

topRefiningCtx :: Module Type -> Either RefiningError RefiningCtx
topRefiningCtx mdl =
  -- TODO: gather up all the RefinedTypes and make the map from their tyIds
  error "TODO"

data RefiningEnv = RefiningEnv {}

topRefiningEnv :: Module Type -> Either RefiningError RefiningCtx
topRefiningEnv mdl = error "TODO"

-- | Utilities
-- lookupFunction :: TermId -> RefiningM (Function Type)
-- lookupFunction funId =
--   asks (Map.lookup funId . ctxFunctions) >>= \case
--     Nothing -> FlexBug.throw $ FlexLog "refining" ("unknown function id" <+> pPrint funId)
--     Just fun -> return fun
