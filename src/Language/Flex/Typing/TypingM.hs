{-# LANGUAGE TemplateHaskell #-}

module Language.Flex.Typing.TypingM where

import Control.Lens hiding (enum)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (Bifunctor (second))
import qualified Data.Map as Map
import Language.Flex.FlexM
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Syntax as Syntax
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility
import Prelude hiding (Enum)

-- * TypingM

type TypingM = StateT TypingEnv (ReaderT TypingCtx (ExceptT TypingError FlexM))

runTypingM :: TypingEnv -> TypingCtx -> TypingM a -> FlexM (Either TypingError (a, TypingEnv))
runTypingM env ctx m = runExceptT $ runReaderT (runStateT m env) ctx

runTypingM' :: (MonadError TypingError m, MonadFlex m) => TypingEnv -> TypingCtx -> TypingM a -> m a
runTypingM' env ctx m =
  liftFlex (runExceptT (runReaderT (evalStateT m env) ctx)) >>= \case
    Left err -> throwError err
    Right a -> return a

-- | During typing, store each type as a `MType = TypingM Type` so that whenever
-- a `Type`'s value is used, the `TypingM` must be run first (in a `TypingM`
-- computation), which normalizes the internal `Type` via the contextual
-- unification substitution.
type MType = TypingM Type

data TypingCtx = TypingCtx
  { _ctxTypes :: Map.Map TypeId (CtxType MType),
    _ctxFunctions :: Map.Map TermId (Function MType ()),
    _ctxConstants :: Map.Map TermId (Constant MType ()),
    _ctxRefinedTypes :: Map.Map TypeId (RefinedType ()),
    _ctxApplicants :: Map.Map (ProtoApplicant ()) (Applicant MType),
    -- | Map of contextual parameter newtype ids (introduced by transform's
    -- contextual parameters)
    _ctxCxparamNewtypeIds :: Map.Map TermId TypeId,
    -- | Map of contextual parameter term ids (introduced by transform's
    -- contextual parameters)
    _ctxCxparamIds :: Map.Map TypeId TermId
  }

data TypingEnv = TypingEnv
  { -- | Current unifying substitution
    _envUnification :: Map.Map UnifyVar Type,
    _envFreshUnificationVarIndex :: Int
  }

data TypingError = TypingError Doc (Maybe (Syntax Type ()))

instance Pretty TypingError where
  pPrint (TypingError msg mb_src) =
    text "typing error:"
      <+> msg
      $$ maybe mempty (nest 2 . pPrint) mb_src

-- *** CtxType

data CtxType ann
  = CtxStructure (Structure ann)
  | CtxNewtype (Newtype ann)
  | CtxVariant (Variant ann)
  | CtxEnum (Enum ann)
  | CtxAlias (Alias ann)

instance Pretty (CtxType ann) where
  pPrint ct = pPrint (ctxTypeId ct)

fromCtxTypeToDeclaration :: CtxType ty -> Declaration ty tm
fromCtxTypeToDeclaration (CtxStructure struc) = toDeclaration struc
fromCtxTypeToDeclaration (CtxNewtype new) = toDeclaration new
fromCtxTypeToDeclaration (CtxVariant vari) = toDeclaration vari
fromCtxTypeToDeclaration (CtxEnum en) = toDeclaration en
fromCtxTypeToDeclaration (CtxAlias al) = toDeclaration al

class IsCtxType a ann where
  toCtxType :: a -> CtxType ann

instance IsCtxType (Structure ann) ann where
  toCtxType = CtxStructure

instance IsCtxType (Newtype ann) ann where
  toCtxType = CtxNewtype

instance IsCtxType (Variant ann) ann where
  toCtxType = CtxVariant

instance IsCtxType (Enum ann) ann where
  toCtxType = CtxEnum

instance IsCtxType (Alias ann) ann where
  toCtxType = CtxAlias

ctxTypeId :: CtxType ann -> TypeId
ctxTypeId (CtxStructure Structure {..}) = structureId
ctxTypeId (CtxNewtype Newtype {..}) = newtypeId
ctxTypeId (CtxVariant Variant {..}) = variantId
ctxTypeId (CtxEnum Enum {..}) = enumId
ctxTypeId (CtxAlias Alias {..}) = aliasId

makeLenses 'TypingCtx
makeLenses 'TypingEnv

-- *** Utilities

modifyInsertUnique :: (MonadError TypingError m, MonadState s m, Pretty a) => Maybe (Syntax Type ()) -> a -> Lens' s (Maybe b) -> b -> m ()
modifyInsertUnique mb_syn a l b = modifyM \s -> case s ^. l of
  Nothing -> return $ (l ?~ b) s
  Just _ -> throwError (TypingError ("attempted to shadow" <+> ticks (pPrint a)) mb_syn)

-- ** Normalization

normalizeType :: Type -> MType
normalizeType = go []
  where
    go aliasIds type0 = do
      FlexM.debug True $ "normalizeType.go" <+> pPrint type0
      case type0 of
        (TypeArray ty) -> TypeArray <$> normalizeType ty
        (TypeTuple tys) -> TypeTuple <$> normalizeType `traverse` tys
        (TypeOptional ty) -> TypeArray <$> normalizeType ty
        (TypeNamed tyId) ->
          asks (^. ctxTypes . at tyId) >>= \case
            Nothing -> return (TypeNamed tyId)
            Just ctxType -> case ctxType of
              (CtxAlias Alias {..}) -> go (aliasId : aliasIds) =<< aliasType
              _ -> return (TypeNamed tyId)
        (TypeUnifyVar uv _) ->
          gets (^. envUnification . at uv) >>= \case
            Nothing -> return type0
            (Just ty) -> normalizeType ty
        _ -> return type0

normalizeInternalTypes :: Traversable t => t MType -> TypingM (t Type)
normalizeInternalTypes = traverse (>>= assertNormalType)

assertNormalType :: Type -> TypingM Type
assertNormalType ty = case ty of
  (TypeNumber {}) -> return ty
  TypeBit -> return ty
  TypeChar -> return ty
  (TypeArray ty') -> TypeArray <$> assertNormalType ty'
  (TypeTuple tys) -> TypeTuple <$> assertNormalType `traverse` tys
  (TypeOptional ty') -> TypeOptional <$> assertNormalType ty'
  (TypeNamed ti) ->
    asks (^. ctxTypes . at ti) >>= \case
      Nothing -> FlexM.throw ("assertNormalType: unknown type id" <+> ticks (pPrint ti))
      (Just ct) -> case ct of
        (CtxAlias {}) -> FlexM.throw ("assertNormalType: type alias reference" <+> ticks (pPrint ty))
        _ -> return ty
  (TypeUnifyVar uv _) ->
    gets (^. envUnification . at uv) >>= \case
      Nothing -> return ty
      (Just {}) -> FlexM.throw ("assertNormalType: unsubstituted type unification variable:" <+> ticks (pPrint ty))

-- ** Defaulting

-- | Abstract types (such as type unification variables) can be defaulted. This
-- is relevant when an internal type is not concretized during typing. This
-- should yield a type that has no more type unification variables in it.
defaultType :: Type -> TypingM Type
defaultType ty = do
  ty' <- go ty
  assertConcreteType ty'
  return ty'
  where
    go :: Type -> TypingM Type
    go (TypeArray ty2) = TypeArray <$> defaultType ty2
    go (TypeTuple tys) = TypeTuple <$> defaultType `traverse` tys
    go (TypeOptional ty2) = TypeOptional <$> defaultType ty2
    go ty2@(TypeUnifyVar _uv Nothing) = FlexM.throw $ "cannot default unconstrained type unification variable" <+> ticks (pPrint ty2)
    go (TypeUnifyVar _uv (Just (UnifyConstraintCasted ty'))) = return ty'
    go (TypeUnifyVar _uv (Just UnifyConstraintNumeric)) = return (TypeNumber TypeInt 32)
    go ty2 = return ty2

defaultInternalTypes :: Traversable t => t Type -> TypingM (t Type)
defaultInternalTypes = traverse (defaultType >=> assertConcreteType)

assertConcreteType :: Type -> TypingM Type
assertConcreteType (TypeArray ty') = TypeArray <$> assertConcreteType ty'
assertConcreteType (TypeTuple tys) = TypeTuple <$> assertConcreteType `traverse` tys
assertConcreteType (TypeOptional ty') = TypeOptional <$> assertConcreteType ty'
assertConcreteType ty@(TypeUnifyVar {}) = FlexM.throw $ "assertConcreteType: found type unification variable" <+> ticks (pPrint ty)
assertConcreteType ty = return ty

-- ** Utilities

introLocal :: Maybe (Syntax Type ()) -> TermId -> MType -> TypingM a -> TypingM a
introLocal mb_syn tmId mty = do
  let mty' = assertNormalType =<< mty
  let protoapp =
        ProtoApplicant
          { protoApplicantMaybeTypeId = Nothing,
            protoApplicantTermId = tmId
          }
  let app =
        Applicant
          { applicantTermId = tmId,
            applicantOutputAnn = mty'
          }
  localM . execStateT $
    modifyInsertUnique mb_syn tmId (ctxApplicants . at protoapp) app

lookupApplicant :: ProtoApplicant () -> TypingM (Applicant MType)
lookupApplicant protoapp =
  asks (^. ctxApplicants . at protoapp) >>= \case
    Nothing -> throwTypingError ("unknown applicant:" <+> ticks (pPrint protoapp)) Nothing
    Just app -> return app

lookupType :: TypeId -> TypingM (CtxType MType)
lookupType tyId =
  asks (^. ctxTypes . at tyId)
    >>= \case
      Nothing -> throwTypingError ("unknown type id" <+> ticks (pPrint tyId)) Nothing
      Just tyM -> return tyM

lookupFunctionType :: TermId -> TypingM (FunctionType MType)
lookupFunctionType tmId =
  asks (^. ctxFunctions . at tmId) >>= \case
    Nothing -> throwTypingError ("unknown function type id" <+> ticks (pPrint tmId)) Nothing
    Just Function {..} -> return functionType

throwTypingError :: MonadError TypingError m => Doc -> Maybe (Syntax Type ()) -> m b
throwTypingError err mb_syn = throwError $ TypingError err mb_syn
