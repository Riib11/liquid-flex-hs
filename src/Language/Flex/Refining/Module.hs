module Language.Flex.Refining.Module where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.Functor
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Traversable
import Language.Flex.FlexM (MonadFlex)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Prelude (preludeRefiningCtx)
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import Language.Flex.Refining.Translating
import qualified Language.Flex.Syntax as Crude
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility hiding (for)

-- * Refining a Module

-- ** Module Refining Context

moduleRefiningCtx ::
  (MonadError RefiningError m, MonadFlex m) =>
  Crude.Module Crude.Type Crude.Type ->
  m RefiningCtx
moduleRefiningCtx Crude.Module {..} = FlexM.markSection [FlexM.FlexMarkStep "moduleRefiningCtx" Nothing] do
  -- collect all type refinements, which will be used to annotate
  -- structures/newtype that are accumulated below
  let refinedTypes = foldr321 moduleDeclarations mempty \case
        (Crude.DeclarationRefinedType Crude.RefinedType {..}) ->
          Map.insert refinedTypeId refinedTypeRefinement
        _ -> id

  let ctx :: RefiningCtx
      ctx = preludeRefiningCtx

  let introStructure structId ctorId fields = do
        fields' <- FlexM.liftFlex $ fields <&*> secondM transType
        reft <- case refinedTypes Map.!? structId of
          Nothing -> FlexM.throw $ "while collecting structures/newtypes before refining, found unknown structure/newtype id:" <+> pPrint structId
          Just reft -> return reft
        modifying (ctxStructures . at structId) . const . Just $
          Structure
            { structureId = structId,
              structureConstructorId = ctorId,
              structureFields = fields',
              structureRefinement = reft
            }
  let introVariant varntId ctors = do
        ctors' <- FlexM.liftFlex $ ctors <&*> secondM (<&*> transType)
        modifying (ctxVariants . at varntId) . const . Just $
          Variant
            { variantId = varntId,
              variantConstructors = ctors'
            }

  flip execStateT ctx . forM_ moduleDeclarations $ \case
    (Crude.DeclarationStructure Crude.Structure {..}) ->
      introStructure structureId (Crude.fromStructureIdToTermId structureId) structureFields
    (Crude.DeclarationNewtype Crude.Newtype {..}) ->
      introStructure newtypeId newtypeConstructorId [(newtypeFieldId, newtypeType)]
    (Crude.DeclarationVariant Crude.Variant {..}) ->
      introVariant variantId variantConstructors
    (Crude.DeclarationEnum Crude.Enum {..}) ->
      introVariant enumId (enumConstructors <&> second (const []))
    (Crude.DeclarationAlias {}) -> do
      -- already handled in typing phase
      return ()
    (Crude.DeclarationFunction Crude.Function {..})
      | Crude.FunctionType {..} <- functionType -> do
          params <- FlexM.liftFlex $ functionParameters <&*> secondM transType
          let mb_cxparams =
                functionContextualParameters
                  <&&> \(newtyId, paramId) -> (paramId, TypeNamed newtyId)
          let params' = params <> fromMaybe [] mb_cxparams
          output' <- FlexM.liftFlex $ transType functionOutput
          modifying (ctxFunctions . at functionId) . const . Just $
            Function
              { functionId,
                functionIsTransform,
                functionParameters = params',
                functionOutput = output',
                functionBody
              }
    (Crude.DeclarationConstant Crude.Constant {..}) ->
      modifying (ctxConstants . at constantId) . const . Just $ constantBody
    (Crude.DeclarationRefinedType {}) -> do
      -- already handled when annotating structures/newtypes with refinements
      return ()

-- ** Module Refining Environment

moduleRefiningEnv :: MonadFlex m => m RefiningEnv
moduleRefiningEnv =
  return
    RefiningEnv
      { _envXXX = "is there anything needed in the environment?"
      }
