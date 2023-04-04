{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Language.Flex.Refining where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Language.Flex.FlexM (MonadFlex)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Checking
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
  mdl' <-
    FlexM.liftFlex $
      (Crude.traverseTm transType >=> Crude.traverseTy transType) mdl
  env <- moduleRefiningEnv
  ctx <- moduleRefiningCtx mdl'
  runRefiningM env ctx . runCheckingM $ do
    -- check transforms
    lift (asks (^. ctxFunctions . to Map.elems . to (filter \Function {..} -> functionIsTransform)))
      >>= traverse_ checkTransform
    -- check constants
    lift (asks (^. ctxConstants . to Map.elems))
      >>= traverse_ checkConstant
    get
