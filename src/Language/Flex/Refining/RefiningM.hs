{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

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

runRefiningM :: (MonadError RefiningError m, MonadFlex m) => RefiningEnv -> RefiningCtx -> RefiningM a -> m a
runRefiningM env ctx m =
  liftFlex (runExceptT (runReaderT (evalStateT m env) ctx)) >>= \case
    Left err -> throwError err
    Right a -> return a

-- ** Refining Context

data RefiningCtx = RefiningCtx
  { _ctxStructures :: Map.Map Crude.TypeId Structure,
    _ctxVariants :: Map.Map Crude.TypeId Variant,
    _ctxFunctions :: Map.Map Crude.TermId Function,
    -- | This includes module-level constants.
    _ctxConstants :: Map.Map Crude.TermId (Crude.Term Type)
  }

-- ** Refining Environment

data RefiningEnv = RefiningEnv
  { _envXXX :: String
  }

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

throwRefiningError :: MonadError RefiningError m => Doc -> m a
throwRefiningError doc = throwError $ RefiningError doc

-- ** Query

-- | The query includes all of the constraints gathered up during
-- checking/synthesizing.
type Query = H.Query RefiningError

_qQuals :: Functor f => (_ -> f _) -> _ -> f _
_qQuals = lens H.qQuals (\q qQuals -> q {H.qQuals = qQuals})

_qVars :: Functor f => (_ -> f _) -> _ -> f _
_qVars = lens H.qVars (\q qVars -> q {H.qVars = qVars})

_qCstr :: Functor f => (_ -> f _) -> _ -> f _
_qCstr = lens H.qCstr (\q qCstr -> q {H.qCstr = qCstr})

_qCon :: Functor f => (_ -> f _) -> _ -> f _
_qCon = lens H.qCon (\q qCon -> q {H.qCon = qCon})

_qDis :: Functor f => (_ -> f _) -> _ -> f _
_qDis = lens H.qDis (\q qDis -> q {H.qDis = qDis})

_qEqns :: Functor f => (_ -> f _) -> _ -> f _
_qEqns = lens H.qEqns (\q qEqns -> q {H.qEqns = qEqns})

_qMats :: Functor f => (_ -> f _) -> _ -> f _
_qMats = lens H.qMats (\q qMats -> q {H.qMats = qMats})

_qData :: Functor f => (_ -> f _) -> _ -> f _
_qData = lens H.qData (\q qData -> q {H.qData = qData})

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

makeLenses ''RefiningCtx
makeLenses ''RefiningEnv

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

lookupFunction funId =
  asks (^. ctxFunctions . at funId) >>= \case
    Nothing -> FlexM.throw $ "unknown function id:" <+> ticks (pPrint funId)
    Just fun -> return fun

typeIsStructure (TypeNamed tyId) =
  asks (^. ctxStructures . at tyId) >>= \case
    Nothing -> return False
    Just _ -> return True
typeIsStructure _ = return False