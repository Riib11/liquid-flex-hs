module Language.Flex.Refining.RefiningM where

import Control.Lens hiding (enum)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (Bifunctor (second))
import qualified Data.Map as Map
import Language.Flex.FlexM
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Syntax
import qualified Language.Flex.Syntax as Crude
import Language.Flex.Typing.TypingM (CtxType (..))
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility
import Prelude hiding (Enum)

type RefiningM = StateT RefiningEnv (ReaderT RefiningCtx (ExceptT RefiningError FlexM))

data RefiningCtx = RefiningCtx
  { _ctxTypes :: Map.Map Crude.TypeId (CtxType Crude.Type),
    _ctxRefinedTypes :: Map.Map Crude.TypeId (Crude.RefinedType Crude.Type),
    _ctxFunctions :: Map.Map Crude.TermId (Crude.Function Crude.Type Crude.Type),
    _ctxConstants :: Map.Map Crude.TermId (Crude.Constant Crude.Type Crude.Type),
    _ctxBindings :: Map.Map Crude.TermId Term
  }

data RefiningEnv = RefiningEnv {}

data RefiningError