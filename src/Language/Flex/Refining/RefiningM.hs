{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Flex.Refining.RefiningM where

import Control.DeepSeq (NFData)
import Control.Lens
import Control.Monad
import Control.Monad.Except (ExceptT, MonadError (throwError), MonadTrans (lift))
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.State (StateT, gets, modify)
import qualified Data.Map as Map
import GHC.Generics
import qualified Language.Fixpoint.Parse as FP
import qualified Language.Fixpoint.Types as F
import Language.Fixpoint.Types.PrettyPrint (pprint)
import qualified Language.Flex.FlexBug as FlexBug
import Language.Flex.FlexM (FlexLog (FlexLog), FlexM)
import Language.Flex.Refining.Syntax
import qualified Language.Flex.Syntax as Base
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

throwRefiningError :: Doc -> RefiningM a
throwRefiningError msg = throwError $ RefiningError msg

-- TODO: refined structures and newtypes
data RefiningCtx = RefiningCtx
  { _ctxTermVars :: Map.Map Base.TermId Type
  }

data RefiningEnv = RefiningEnv
  { _freshSymbolIndex :: Int
  }

makeLenses ''RefiningCtx
makeLenses ''RefiningEnv

topRefiningCtx :: Base.Module Base.Type -> ExceptT RefiningError FlexM RefiningCtx
topRefiningCtx _mdl = do
  return
    RefiningCtx
      { _ctxTermVars = mempty
      }

topRefiningEnv :: Base.Module Base.Type -> ExceptT RefiningError FlexM RefiningEnv
topRefiningEnv _mdl = do
  return
    RefiningEnv
      { _freshSymbolIndex = 0
      }

-- ** Utilities

lookupTermId :: Base.TermId -> RefiningM Type
lookupTermId tmId =
  asks (^. ctxTermVars . at tmId)
    >>= \case
      Nothing -> FlexBug.throw $ FlexLog "refining" $ "unknown term symbol:" <+> pPrint tmId
      Just ty -> return ty

fromTermIdToSymbol :: Base.TermId -> RefiningM F.Symbol
fromTermIdToSymbol = error "fromTermIdToSymbol"

freshSymbol :: String -> RefiningM F.Symbol
freshSymbol str = do
  i <- gets (^. freshSymbolIndex)
  modifying freshSymbolIndex (1 +)
  return $ parseSymbol (str <> "#" <> show i)

parseSymbol :: String -> F.Symbol
parseSymbol str = FP.doParse' FP.lowerIdP ("parseSymbol: " <> str) str

parsePred :: String -> F.Pred
parsePred = FP.doParse' FP.predP "parsePred"