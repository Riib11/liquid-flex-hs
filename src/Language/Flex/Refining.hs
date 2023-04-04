{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Language.Flex.Refining where

import Control.Monad.Except
import Control.Monad.State
import Language.Flex.FlexM (MonadFlex)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Module
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import Language.Flex.Refining.Translating
import qualified Language.Flex.Syntax as Crude
import Utility hiding (for)

-- !TODO also return a refined Module maybe?
refineModule ::
  (MonadError RefiningError m, MonadFlex m) =>
  Crude.Module Crude.Type Crude.Type ->
  m RefiningEnv
refineModule mdl = do
  mdl'@Crude.Module {..} <-
    FlexM.liftFlex $
      (Crude.traverseTm transType >=> Crude.traverseTy transType) mdl
  env <- moduleRefiningEnv
  ctx <- moduleRefiningCtx mdl'
  runRefiningM env ctx do
    moduleDeclarations <&*> \case
      (Crude.DeclarationFunction fun@Crude.Function {..})
        | Crude.functionIsTransform functionType ->
            checkTransform fun
      (Crude.DeclarationConstant con) ->
        checkConstant con
      (Crude.DeclarationRefinedType rt) ->
        checkRefinedType rt
      _ -> return ()
    get

checkTransform :: Crude.Function Type Type -> RefiningM ()
checkTransform = error "checkTransform"

checkConstant :: Crude.Constant Type Type -> RefiningM ()
checkConstant = error "checkConstant"

checkRefinedType :: Crude.RefinedType Type -> RefiningM ()
checkRefinedType = error "checkRefinedType"
