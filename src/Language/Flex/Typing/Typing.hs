module Language.Flex.Typing.Typing where

import Control.Lens hiding (enum)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (Bifunctor (second))
import qualified Data.Map as Map
import Language.Flex.FlexM
import Language.Flex.Syntax as Syntax
import Language.Flex.Typing.TypingM
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility
import Prelude hiding (Enum, enum)

-- ** Top Typing Context

-- | Constructs the top typing context of a module, which involves the
-- following:
-- - structures
--   - intro extended structure type
-- - newtypes
--   - intro newtype type
--   - intro newtype applicant
-- - variants
--   - intro variant type
--   - intro variant constructors
-- - enums
--   - intro enum type
--   - intro enum constructors
-- - aliases
--   - intro alias
-- - functions
--   - intro function
--   - intro function applicant
-- - constants
--   - intro constant applicant
-- - refined types
--   - !TODO
topTypingCtx :: (MonadError TypingError m, MonadFlex m) => Module Type () -> m TypingCtx
topTypingCtx Module {..} = do
  -- collect all structures
  let structs :: Map.Map TypeId (Structure Type)
      structs = for moduleDeclarations mempty \case
        (DeclarationStructure struct@Structure {..}) -> Map.insert structureId struct
        _ -> id

  let ctx :: TypingCtx
      ctx = undefined

  let -- fully extend the structure's fields
      extendNormStructure struct = undefined

  flip execStateT ctx . forM_ moduleDeclarations $ \case
    (DeclarationStructure struct@Structure {..}) -> do
      let struct' = normType <$> extendNormStructure struct
      ctxTypes . at structureId ?= CtxStructure struct'
    (DeclarationNewtype newty@Newtype {..}) -> do
      let newty' = normType <$> newty
      ctxTypes . at newtypeId ?= CtxNewtype newty'
    (DeclarationVariant vari) -> do
      -- intro type
      let vari'@Variant {..} = normType <$> vari
      ctxTypes . at variantId ?= CtxVariant vari'
      -- intro constructors
      forM_ variantConstructors \(ctorId, _) -> do
        let apl =
              Applicant
                { applicantMaybeTypeId = Just variantId,
                  applicantTermId = ctorId,
                  applicantAnn = ApplicantTypeVariantConstructor variantId ctorId
                }
        ctxApplicants . at (void apl) ?= apl
    (DeclarationEnum enum) -> do
      -- intro type
      let enum'@Enum {..} = normType <$> enum
      ctxTypes . at enumId ?= CtxEnum enum'
      -- intro constructors
      forM_ enumConstructors \(ctorId, _) -> do
        let apl =
              Applicant
                { applicantMaybeTypeId = Just enumId,
                  applicantTermId = ctorId,
                  applicantAnn = ApplicantTypeVariantConstructor enumId ctorId
                }
        ctxApplicants . at (void apl) ?= apl
    (DeclarationAlias alias) -> do
      -- intro type
      let alias'@Alias {..} = normType <$> alias
      ctxTypes . at aliasId ?= CtxAlias alias'
    (DeclarationFunction fun) -> do
      -- intro function
      let fun'@Function {..} = fmapTy normType fun
      ctxFunctions . at functionId ?= fun'
      -- intro applicant
      let apl =
            Applicant
              { applicantMaybeTypeId = Nothing,
                applicantTermId = functionId,
                applicantAnn = ApplicantTypeFunction functionId
              }
      ctxApplicants . at (void apl) ?= apl
    (DeclarationConstant con) -> do
      -- intro constant
      let con'@Constant {..} = fmapTy normType con
      ctxConstants . at constantId ?= con'
      -- intro applicant
      let apl =
            Applicant
              { applicantMaybeTypeId = Nothing,
                applicantTermId = constantId,
                applicantAnn = ApplicantType constantType
              }
      ctxApplicants . at (void apl) ?= apl
    (DeclarationRefinedType rt@RefinedType {..}) -> do
      -- intro refined type
      ctxRefinedTypes . at refinedTypeId ?= rt

-- ** Top Typing Environment

topTypingEnv :: (MonadError TypingError m) => Module Type () -> m TypingEnv
topTypingEnv _ = do
  return
    TypingEnv
      { _envUnification = mempty,
        _envFreshUnificationVarIndex = 0
      }
