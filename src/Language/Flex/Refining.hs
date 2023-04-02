{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Flex.Refining where

import Control.Monad.Except (MonadError)
import Language.Flex.FlexM (MonadFlex)
import Language.Flex.Refining.RefiningM
import Language.Flex.Syntax as Crude

refineModule :: (MonadError RefiningError m, MonadFlex m) => Crude.Module Crude.Type Crude.Type -> m (RefiningEnv, Crude.Module Type Type)
refineModule = error "refineModule"

checkFunction :: Crude.Function Type Type -> RefiningM ()
checkFunction = error "checkFunction"

checkConstant :: Crude.Constant Type Type -> RefiningM ()
checkConstant = error "checkConstant"

checkRefinedType :: Crude.RefinedType Type -> RefiningM ()
checkRefinedType = error "checkRefinedType"
