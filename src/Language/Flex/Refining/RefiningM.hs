{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Language.Flex.Refining.RefiningM where

import Control.DeepSeq (NFData)
import Control.Lens hiding (enum)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (Bifunctor (second))
import qualified Data.Map as Map
import GHC.Generics (Generic)
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Types as F
import Language.Flex.FlexM
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Syntax
import qualified Language.Flex.Syntax as Crude
import Language.Flex.Typing.TypingM (CtxType (..))
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility
import Prelude hiding (Enum)

-- ** RefiningM

type RefiningM = StateT RefiningEnv (ReaderT RefiningCtx (ExceptT RefiningError FlexM))

-- ** Refining Context

data RefiningCtx = RefiningCtx
  { _ctxStructures :: Map.Map Crude.TypeId Structure,
    _ctxVariants :: Map.Map Crude.TypeId Variant,
    _ctxFunctions :: Map.Map Crude.TermId Function,
    -- -- | This includes module-level constants.
    -- _ctxBindings :: Map.Map Crude.TermId TermExpr
    _ctxConstants :: Map.Map Crude.TermId Term
  }

-- ** Refining Environment

data RefiningEnv = RefiningEnv {}

-- ** RefiningError

newtype RefiningError = RefiningError Doc
  deriving (Eq, Show, Generic)

instance NFData RefiningError

instance Pretty RefiningError where
  pPrint (RefiningError msg) = msg

instance F.Fixpoint RefiningError where
  toFix = text . renderInline . pPrint

instance F.Loc RefiningError where
  srcSpan _ = F.dummySpan

instance F.PPrint RefiningError where
  pprintTidy _ = text . renderInline . pPrint

-- ** Query

-- | The query includes all of the constraints gathered up during
-- checking/synthesizing.
type Query = H.Query RefiningError

makeLenses ''RefiningCtx
makeLenses ''RefiningEnv

-- ** Result

--
-- Result received back after submitting query
type Result = F.FixResult RefiningError

-- ** Constraints

--
-- In Liquid Fixpoint, `H.Cstr` has the following form:
--
-- > data Cstr a
-- >   = Head  !Pred !a                  -- ^ p
-- >   | CAnd  ![Cstr a]                 -- ^ c1 /\ ... /\ cn
-- >   | All   !(Bind a)  !(Cstr a)      -- ^ \all x:t. p => c
-- >   | Any   !(Bind a)  !(Cstr a)      -- ^ \exi x:t. p /\ c or is it \exi x:t. p => c?
-- >   deriving (Data, Typeable, Generic, Functor, Eq)
type Cstr = H.Cstr RefiningError

type Bind = H.Bind RefiningError

-- ** Utilities

lookupVariant varntId =
  asks (^. ctxVariants . at varntId) >>= \case
    Nothing -> FlexM.throw $ "unknown variant id:" <+> ticks (pPrint varntId)
    Just varnt -> return varnt

lookupConstructorParameterTypes varntId ctorId = do
  lookupVariant varntId >>= \Variant {..} ->
    case ctorId `lookup` variantConstructors of
      Nothing -> FlexM.throw $ "unknown constructor id:" <+> ticks (pPrint ctorId)
      Just ctorParamTypes -> return ctorParamTypes

lookupStructure structId =
  asks (^. ctxStructures . at structId) >>= \case
    Nothing -> FlexM.throw $ "unknown structure id:" <+> ticks (pPrint structId)
    Just struct -> return struct
