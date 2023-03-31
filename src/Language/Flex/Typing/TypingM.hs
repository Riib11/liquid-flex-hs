{-# LANGUAGE TemplateHaskell #-}

module Language.Flex.Typing.TypingM where

import Control.Lens hiding (enum)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (Bifunctor (second))
import qualified Data.Map as Map
import Language.Flex.FlexM
import Language.Flex.Syntax as Syntax
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility
import Prelude hiding (Enum, enum)

-- * TypingM

type TypingM = StateT TypingEnv (ReaderT TypingCtx (ExceptT TypingError FlexM))

-- | During typing, store each type as a `TypeM = TypingM Type` so that whenever
-- a `Type`'s value is used, the `TypingM` must be run first (in a `TypingM`
-- computation), which normalizes the internal `Type` via the contextual
-- unification substitution.
type TypeM = TypingM Type

data TypingCtx = TypingCtx
  { _ctxTypes :: Map.Map TypeId (CtxType TypeM),
    _ctxFunctions :: Map.Map TermId (Function TypeM ()),
    _ctxConstants :: Map.Map TermId (Constant TypeM ()),
    _ctxRefinedTypes :: Map.Map TypeId (RefinedType ()),
    _ctxApplicants :: Map.Map (Applicant ()) (Applicant TypeM),
    -- | Map of contextual parameter newtype ids (introduced by transform's
    -- contextual parameters)
    _ctxCxparamNewtypeIds :: Map.Map TermId TypeId,
    -- | Map of contextual parameter term ids (introduced by transform's
    -- contextual parameters)
    _ctxCxparamTermIds :: Map.Map TypeId TermId
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

makeLenses 'TypingCtx
makeLenses 'TypingEnv

-- ** Normalization

normType :: Type -> TypeM
normType type_ = case type_ of
  (TypeNumber nt n) -> return type_
  TypeBit -> return type_
  TypeChar -> return type_
  (TypeArray ty) -> TypeArray <$> normType ty
  (TypeTuple tys) -> TypeTuple <$> normType `traverse` tys
  (TypeOptional ty) -> TypeArray <$> normType ty
  --   (TypeNamed ti _) -> TypeNamed ti . Just <$> join (lookupType ti)
  (TypeNamed ti _) ->
    TypeNamed ti
      <$> ( lookupType ti >>= \case
              (CtxAlias Alias {..}) -> Just <$> aliasType
              _ -> return Nothing
          )
  (TypeUnifyVar uv _) ->
    gets (^. envUnification . at uv) >>= \case
      Nothing -> return type_
      (Just ty) -> normType ty

-- ** Utilities

introTerm :: TermId -> TypeM -> TypingM a -> TypingM a
introTerm tmId tyM =
  locallyM (ctxApplicants . at (Applicant Nothing tmId (ApplicantType ()))) \case
    Nothing -> return . pure $ Applicant Nothing tmId (ApplicantType tyM)
    Just _ -> throwTypingError ("attempted to introduce two terms with the same name:" <+> ticks (pPrint tmId)) Nothing

introCxparam :: Maybe (Syntax Type ()) -> TypeId -> TermId -> TypingM a -> TypingM a
introCxparam mb_syn tyId tmId =
  comps
    [ introTerm tmId (normType (TypeNamed tyId Nothing)),
      locallyM (ctxCxparamNewtypeIds . at tmId) \case
        Nothing -> return $ pure tyId
        Just _ -> throwTypingError ("attempted to introduce two contextual parameters with the same name:" <+> ticks (pPrint tmId)) mb_syn,
      locallyM (ctxCxparamTermIds . at tyId) \case
        Nothing -> return $ pure tmId
        Just _ -> throwTypingError ("attempted to introduce two contextual parameters with the same type:" <+> ticks (pPrint tyId)) mb_syn
    ]

lookupApplicant :: Applicant () -> TypingM (Applicant TypeM)
lookupApplicant app =
  asks (^. ctxApplicants . at app) >>= \case
    Nothing -> throwTypingError ("unknown applicant:" <+> ticks (pPrint app)) Nothing
    Just appTyM -> return appTyM

lookupType :: TypeId -> TypingM (CtxType TypeM)
lookupType tyId =
  asks (^. ctxTypes . at tyId)
    >>= \case
      Nothing -> throwTypingError ("unknown type id" <+> ticks (pPrint tyId)) Nothing
      Just tyM -> return tyM

throwTypingError :: MonadError TypingError m => Doc -> Maybe (Syntax Type ()) -> m b
throwTypingError err mb_syn = throwError $ TypingError err mb_syn
