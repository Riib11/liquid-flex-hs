{-# LANGUAGE TemplateHaskell #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant return" #-}

module Language.Flex.Typing.TypingM where

import Control.Lens hiding (enum)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (Bifunctor (second))
import qualified Data.List as List
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

runTypingM_unsafe :: MonadFlex m => TypingEnv -> TypingCtx -> TypingM a -> m a
runTypingM_unsafe env ctx m = liftFlex do
  runExceptT (runReaderT (runStateT m env) ctx) >>= \case
    (Left err) -> FlexM.throw $ pPrint err
    (Right (a, _)) -> return a

data TypingCtx = TypingCtx
  { _ctxTypes :: Map.Map TypeId (CtxType Type),
    _ctxFunctions :: Map.Map TermId (Function Type ()),
    _ctxConstants :: Map.Map TermId (Constant Type ()),
    _ctxRefinedTypes :: Map.Map TypeId (RefinedType ()),
    _ctxApplicants :: Map.Map (ProtoApplicant ()) (Applicant Type),
    -- | Map of contextual parameter newtype ids (introduced by transform's
    -- contextual parameters)
    _ctxCxparamNewtypeIds :: Map.Map TermId TypeId,
    -- | Map of contextual parameter term ids (introduced by transform's
    -- contextual parameters)
    _ctxCxparamIds :: Map.Map TypeId TermId
  }

data TypingEnv = TypingEnv
  { -- | Current unifying substitution
    _envUnification :: Map.Map UnifyVar (Either UnifyConstraint Type),
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

-- defaults unsubstituted type vars
normalizeType :: Type -> TypingM Type
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
              (CtxAlias Alias {..}) | aliasId `elem` aliasIds -> throwTypingError ("cycle of type aliases:" <+> commaList (pPrint <$> aliasIds)) (Just $ SyntaxType (TypeNamed tyId))
              (CtxAlias Alias {..}) -> do
                FlexM.debug True $ "[normalizeType] aliasId =" <+> pPrint aliasId
                go (aliasId : aliasIds) aliasType
              _ -> return (TypeNamed tyId)
        TypeUnifyVar uv ->
          gets (^. envUnification . at uv) >>= \case
            (Just (Right ty)) -> normalizeType ty
            -- _ -> defaultType type0
            _ -> return type0
        _ -> return type0

-- normalizes and defaults
normalizeAndDefaultInternalTypes :: Traversable t => t Type -> TypingM (t Type)
normalizeAndDefaultInternalTypes t0 = do
  t1 <- normalizeType `traverse` t0
  t2 <- defaultInternalTypes t1
  t3 <- assertNormalType `traverse` t2
  return t3

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
  TypeUnifyVar uv ->
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
    go ty2@(TypeUnifyVar uv) =
      gets (^. envUnification . at uv) >>= \case
        Nothing -> FlexM.throw $ "cannot default unconstrained type unification variable" <+> ticks (pPrint ty2)
        (Just (Left uc)) -> case uc of
          (UnifyConstraintCasted ty') -> defaultType ty'
          UnifyConstraintNumeric -> return (TypeNumber TypeInt 32)
        -- the var has been substituted via the environment, so probably should
        -- have already been handled by normalization?
        (Just (Right ty')) -> go ty'
    go ty2 = return ty2

defaultInternalTypes :: Traversable t => t Type -> TypingM (t Type)
defaultInternalTypes = traverse (defaultType >=> assertConcreteType)

assertConcreteType :: Type -> TypingM Type
assertConcreteType (TypeArray ty') = TypeArray <$> assertConcreteType ty'
assertConcreteType (TypeTuple tys) = TypeTuple <$> assertConcreteType `traverse` tys
assertConcreteType (TypeOptional ty') = TypeOptional <$> assertConcreteType ty'
assertConcreteType ty@(TypeUnifyVar {}) = FlexM.throw $ "assertConcreteType: found type unification variable" <+> ticks (pPrint ty)
assertConcreteType ty = return ty

-- ** Typed Term

-- Asserts typed term has been correcly processed.
-- - no PrimitiveCast
-- - no TermAscribe
-- - no ProtoNeutral
assertTypedTerm :: Term Type -> Term Type
assertTypedTerm = error "assertTypedTerm"

-- ** Utilities

introLocal :: Maybe (Syntax Type ()) -> TermId -> Type -> TypingM a -> TypingM a
introLocal mb_syn tmId ty m = do
  ty' <- normalizeType ty
  let protoapp =
        ProtoApplicant
          { protoApplicantMaybeTypeId = Nothing,
            protoApplicantTermId = tmId
          }
  let app =
        Applicant
          { applicantTermId = tmId,
            applicantOutputAnn = ty'
          }
  ( localM . execStateT $
      modifyInsertUnique mb_syn tmId (ctxApplicants . at protoapp) app
    )
    m

lookupApplicant :: ProtoApplicant () -> TypingM (Applicant Type)
lookupApplicant protoapp =
  asks (^. ctxApplicants . at protoapp) >>= \case
    Nothing -> throwTypingError ("unknown applicant:" <+> ticks (pPrint protoapp)) Nothing
    Just app -> return app

lookupType :: TypeId -> TypingM (CtxType Type)
lookupType tyId =
  asks (^. ctxTypes . at tyId)
    >>= \case
      Nothing -> throwTypingError ("unknown type id" <+> ticks (pPrint tyId)) Nothing
      Just tyM -> return tyM

lookupFunctionType :: TermId -> TypingM (FunctionType Type)
lookupFunctionType tmId =
  asks (^. ctxFunctions . at tmId) >>= \case
    Nothing -> throwTypingError ("unknown function type id" <+> ticks (pPrint tmId)) Nothing
    Just Function {..} -> return functionType

throwTypingError :: MonadError TypingError m => Doc -> Maybe (Syntax Type ()) -> m b
throwTypingError err mb_syn = throwError $ TypingError err mb_syn
