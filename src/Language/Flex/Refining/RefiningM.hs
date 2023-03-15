{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Flex.Refining.RefiningM where

import Control.DeepSeq (NFData)
import Control.Lens
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

throwRefiningError :: Doc -> RefiningM a
throwRefiningError msg = throwError $ RefiningError msg

data RefiningCtx = RefiningCtx {}

data RefiningEnv = RefiningEnv
  { _freshSymbolIndex :: Int
  }

makeLenses ''RefiningCtx
makeLenses ''RefiningEnv

topRefiningCtx :: Module Type -> Either RefiningError RefiningCtx
topRefiningCtx mdl =
  -- TODO: gather up all the RefinedTypes and make the map from their tyIds
  error "TODO"

topRefiningEnv :: Module Type -> Either RefiningError RefiningCtx
topRefiningEnv mdl = error "TODO"

-- ** Utilities

freshSymbol :: String -> RefiningM F.Symbol
freshSymbol str = do
  i <- gets (^. freshSymbolIndex)
  modifying freshSymbolIndex (1 +)
  return $ parseSymbol (str <> "#" <> show i)

parseSymbol :: String -> F.Symbol
parseSymbol str = FP.doParse' FP.lowerIdP ("parseSymbol: " <> str) str

parsePred :: String -> F.Pred
parsePred = FP.doParse' FP.predP "parsePred"