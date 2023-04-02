module Language.Flex.Typing.Module where

import Control.Lens hiding (enum)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (Bifunctor (second))
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Language.Flex.FlexM
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Syntax as Syntax
import Language.Flex.Typing.TypingM
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility
import Prelude hiding (Enum, enum)

-- ** Module Typing Context

-- | Constructs the module-level typing context.
moduleTypingCtx :: (MonadError TypingError m, MonadFlex m) => Module Type () -> m TypingCtx
moduleTypingCtx Module {..} = do
  -- collect all structures
  let structs :: Map.Map TypeId (Structure Type)
      structs = for moduleDeclarations mempty \case
        (DeclarationStructure struct@Structure {..}) -> Map.insert structureId struct
        _ -> id

  -- collect all refined types
  let refinedTypes :: Map.Map TypeId (RefinedType ())
      refinedTypes = for moduleDeclarations mempty \case
        DeclarationRefinedType rt@RefinedType {..} -> Map.insert refinedTypeId rt
        _ -> id

  let -- fully extend the structure's fields
      extendStructure structIds struct@Structure {..} = do
        -- assert acyclic extensions
        when (structureId `elem` structIds) $ throwTypingError ("the structure" <+> ticks (pPrint structureId) <+> "has an extension that forms a cycle:" <+> hcat (punctuate (space <> "extends" <> space) $ pPrint <$> reverse structIds)) (Just $ SyntaxDeclaration $ DeclarationStructure struct)

        case structs Map.!? structureId of
          Nothing -> return struct
          Just struct' -> do
            -- extend the extending struct
            struct'' <- extendStructure structIds struct'

            -- assert non-overlapping fields from extension
            case filter (isJust . (`lookup` Syntax.structureFields struct'') . fst) structureFields of
              interFields | not (null interFields) -> throwTypingError ("the structure" <+> ticks (pPrint structureId) <+> "inherits fields that overlap with fields it already has:" <+> pPrint (fst <$> interFields)) (Just $ SyntaxDeclaration $ DeclarationStructure struct)
              _ -> return ()

            -- append extending struct's fields to extended struct's fields
            return struct {structureFields = structureFields <> Syntax.structureFields struct''}

  let -- fully extend the refined type's refinements (if it is refining a structure that extends another structure)
      extendRefinedType tyIds rt@RefinedType {..} = case structs Map.!? refinedTypeId of
        Nothing -> return rt
        Just Structure {..} -> case refinedTypes Map.!? structureId of
          Nothing -> FlexM.throw $ "unknown structure id:" <+> ticks (pPrint structureId)
          Just rt' -> do
            -- extend refinement
            rt'' <- extendRefinedType (refinedTypeId : tyIds) rt'
            -- conjoin refinements
            return
              rt
                { refinedTypeRefinement =
                    andRefinements
                      [ refinedTypeRefinement,
                        Syntax.refinedTypeRefinement rt''
                      ]
                }

  let ctx :: TypingCtx
      ctx =
        TypingCtx
          { _ctxTypes = mempty,
            _ctxFunctions = mempty,
            _ctxConstants = mempty,
            _ctxRefinedTypes = mempty,
            _ctxApplicants = mempty,
            _ctxCxparamNewtypeIds = mempty,
            _ctxCxparamIds = mempty
          }

  flip execStateT ctx . forM_ moduleDeclarations $ \case
    (DeclarationStructure struct@Structure {..}) -> do
      -- extend structure
      struct' <- extendStructure [] struct
      -- normalize structure
      let struct'' = normalizeType <$> struct'
      -- intro structure type
      ctxTypes . at structureId ?= CtxStructure struct''
    (DeclarationNewtype newty@Newtype {..}) -> do
      -- normalize newtype
      let newty' = normalizeType <$> newty
      -- intro newtype type
      ctxTypes . at newtypeId ?= CtxNewtype newty'
      -- intro constructor applicant
      -- let app = Applicant
      --       { applicant

      --       }
      -- ctxApplicants . at _ ?= _
      error "intro constructor applicant"
    (DeclarationVariant vari) -> do
      -- intro normalize type
      let vari'@Variant {..} = normalizeType <$> vari
      ctxTypes . at variantId ?= CtxVariant vari'
      -- intro constructors' applicants
      forM_ variantConstructors \(ctorId, _) -> do
        let apl =
              Applicant
                { applicantMaybeTypeId = Just variantId,
                  applicantTermId = ctorId,
                  applicantAnn = ApplicantTypeVariantConstructor variantId ctorId
                }
        ctxApplicants . at (void apl) ?= apl
    (DeclarationEnum enum) -> do
      -- intro normalize type
      let enum'@Enum {..} = normalizeType <$> enum
      ctxTypes . at enumId ?= CtxEnum enum'
      -- intro constructors' applicants
      forM_ enumConstructors \(ctorId, _) -> do
        let apl =
              Applicant
                { applicantMaybeTypeId = Just enumId,
                  applicantTermId = ctorId,
                  applicantAnn = ApplicantTypeVariantConstructor enumId ctorId
                }
        ctxApplicants . at (void apl) ?= apl
    (DeclarationAlias alias) -> do
      -- intro normalize type
      let alias'@Alias {..} = normalizeType <$> alias
      ctxTypes . at aliasId ?= CtxAlias alias'
    (DeclarationFunction fun) -> do
      -- intro normalized function
      let fun'@Function {..} = fmapTy normalizeType fun
      let FunctionType {..} = functionType
      ctxFunctions . at functionId ?= fun'
      -- intro application applicant
      let apl =
            Applicant
              { applicantMaybeTypeId = Nothing,
                applicantTermId = functionId,
                applicantAnn = ApplicantTypeFunction functionId
              }
      ctxApplicants . at (void apl) ?= apl
    (DeclarationConstant con) -> do
      -- intro constant
      let con'@Constant {..} = fmapTy normalizeType con
      ctxConstants . at constantId ?= con'
      -- intro named applicant
      let apl =
            Applicant
              { applicantMaybeTypeId = Nothing,
                applicantTermId = constantId,
                applicantAnn = ApplicantType constantType
              }
      ctxApplicants . at (void apl) ?= apl
    (DeclarationRefinedType rt@RefinedType {..}) -> do
      -- extend refined type (if refined structure inherits refinements)
      rt' <- extendRefinedType [] rt
      -- intro refined type
      ctxRefinedTypes . at refinedTypeId ?= rt'

-- ** Module Typing Environment

moduleTypingEnv :: (MonadError TypingError m) => Module Type () -> m TypingEnv
moduleTypingEnv _ = do
  return
    TypingEnv
      { _envUnification = mempty,
        _envFreshUnificationVarIndex = 0
      }
