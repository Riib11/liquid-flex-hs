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
import Language.Flex.Typing.Prelude (preludeTypingCtx)
import Language.Flex.Typing.TypingM
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility
import Prelude hiding (Enum)

-- ** Module Typing Context

-- | Constructs the module-level typing context.
moduleTypingCtx ::
  (MonadError TypingError m, MonadFlex m) =>
  Module Type () ->
  m TypingCtx
moduleTypingCtx Module {..} = FlexM.markSection [FlexM.FlexMarkStep "moduleTypingCtx" Nothing] do
  let ctx :: TypingCtx
      ctx = preludeTypingCtx

  flip execStateT ctx . forM_ moduleDeclarations $ \decl -> case decl of
    (DeclarationStructure struct@Structure {..}) -> do
      -- extend structure
      -- normalize structure
      let struct'' = normalizeType <$> struct
      -- intro structure type
      modifyInsertUnique (Just $ SyntaxDeclaration decl) structureId (ctxTypes . at structureId) (CtxStructure struct'')
    (DeclarationNewtype newty@Newtype {..}) -> do
      -- normalize newtype
      let newty' = normalizeType <$> newty
      -- intro newtype type
      modifyInsertUnique (Just $ SyntaxDeclaration decl) newtypeId (ctxTypes . at newtypeId) (CtxNewtype newty')
      -- intro constructor applicant
      let app =
            ApplicantNewtypeConstructor
              { applicantNewtypeId = newtypeId,
                applicantConstructorId = newtypeConstructorId,
                applicantOutputAnn = return $ TypeNamed newtypeId
              }
      let protoapp = fromApplicantToProtoApplicant app
      modifyInsertUnique Nothing (void app) (ctxApplicants . at protoapp) app
    (DeclarationVariant vari) -> do
      -- intro normalize type
      let vari'@Variant {..} = normalizeType <$> vari
      modifyInsertUnique (Just $ SyntaxDeclaration decl) variantId (ctxTypes . at variantId) (CtxVariant vari')
      -- intro constructors' applicants
      forM_ variantConstructors \(ctorId, _) -> do
        let app =
              ApplicantVariantConstructor
                { applicantVariantId = variantId,
                  applicantConstructorId = ctorId,
                  applicantOutputAnn = return $ TypeNamed variantId
                }
        let protoapp = fromApplicantToProtoApplicant app
        modifyInsertUnique Nothing (void app) (ctxApplicants . at protoapp) app
    (DeclarationEnum enum) -> do
      -- intro normalize type
      let enum'@Enum {..} = normalizeType <$> enum
      modifyInsertUnique (Just $ SyntaxDeclaration decl) enumId (ctxTypes . at enumId) (CtxEnum enum')
      -- intro constructors' applicants
      forM_ enumConstructors \(ctorId, _) -> do
        let app =
              ApplicantEnumConstructor
                { applicantEnumId = enumId,
                  applicantConstructorId = ctorId,
                  applicantOutputAnn = return $ TypeNamed enumId
                }
        let protoapp = fromApplicantToProtoApplicant app
        modifyInsertUnique Nothing (void app) (ctxApplicants . at protoapp) app
    (DeclarationAlias alias) -> do
      -- intro normalize type
      let alias'@Alias {..} = normalizeType <$> alias
      modifyInsertUnique (Just $ SyntaxDeclaration decl) aliasId (ctxTypes . at aliasId) (CtxAlias alias')
    (DeclarationFunction fun) -> do
      -- intro normalized function
      let fun'@Function {..} = fmapTy normalizeType fun
      let FunctionType {..} = functionType
      modifyInsertUnique (Just $ SyntaxDeclaration decl) functionId (ctxFunctions . at functionId) fun'
      -- intro application applicant
      let app =
            ApplicantFunction
              { applicantFunctionId = functionId,
                applicantOutputAnn = functionOutput
              }
      let protoapp = fromApplicantToProtoApplicant app
      modifyInsertUnique Nothing (void app) (ctxApplicants . at protoapp) app
    (DeclarationConstant con) -> do
      -- intro constant
      let con'@Constant {..} = fmapTy normalizeType con
      modifyInsertUnique (Just $ SyntaxDeclaration decl) constantId (ctxConstants . at constantId) con'
      -- intro named applicant
      let app =
            Applicant
              { applicantTermId = constantId,
                applicantOutputAnn = constantType
              }
      let protoapp = fromApplicantToProtoApplicant app
      modifyInsertUnique (Just $ SyntaxDeclaration decl) (void app) (ctxApplicants . at protoapp) app
    (DeclarationRefinedType rt@RefinedType {..}) -> do
      -- extend refined type (if refined structure inherits refinements)
      -- intro refined type
      modifyInsertUnique (Just $ SyntaxDeclaration decl) refinedTypeId (ctxRefinedTypes . at refinedTypeId) rt

-- ** Module Typing Environment

moduleTypingEnv :: (MonadError TypingError m) => Module Type () -> m TypingEnv
moduleTypingEnv _ = do
  return
    TypingEnv
      { _envUnification = mempty,
        _envFreshUnificationVarIndex = 0
      }
