{-# LANGUAGE TemplateHaskell #-}

module Flex.Refining.Common where

import Control.Lens
import Control.Lens (makeLenses)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT), asks, foldM)
import Control.Monad.State.Class (MonadState (get))
import qualified Data.Map as Map
import Data.Text (unpack)
import Flex.Flex
import Flex.Refining.Syntax
import qualified Flex.Syntax as Base
import qualified Language.Fixpoint.Types as F

-- | Refining monad
type Refining a = ReaderT Ctx FlexM a

runRefining :: Refining a -> FlexM a
runRefining m = runReaderT m =<< initCtx

runRefiningWith :: Ctx -> Refining a -> FlexM a
runRefiningWith ctx m = runReaderT m ctx

throwRefiningError :: String -> Refining a
throwRefiningError = throwError . RefineError . MakeRefineError

throwRefiningBug :: String -> Refining a
throwRefiningBug msg = lift $ throwBug ("refining", msg)

-- | Ctx
data Ctx = Ctx
  { _idSymbols :: Map.Map Base.Id F.Symbol,
    _senv :: F.SEnv Type
  }

initCtx :: FlexM Ctx
initCtx = do
  env <- get
  -- need to iterate over functions, constants, and constructors in the current
  -- module context
  idSyms <-
    flip
      ( foldM . flip $ \fun m -> do
          s <- freshSymbol (unpack $ Base.functionName fun)
          return $ Map.insert (Base.fromUnqualName (Base.functionName fun)) s m
      )
      (env ^. envModuleCtx . Base.ctxModuleFunctions)
      =<< flip
        ( foldM . flip $ \con m -> do
            let n = Base.constantName con
            s <- freshSymbol (unpack n)
            return $ Map.insert (Base.fromUnqualName n) s m
        )
        (env ^. envModuleCtx . Base.ctxModuleConstants)
      =<< flip
        ( foldM . flip $ \cnstr m -> do
            let n = Base.constructorName cnstr
            s <- freshSymbol (unpack n)
            return $ Map.insert (Base.fromUnqualName n) s m
        )
        (env ^. envModuleCtx . Base.ctxModuleConstructors)
      =<< return Map.empty
  return
    Ctx
      { _idSymbols = idSyms,
        _senv = F.emptySEnv
      }

makeLenses ''Ctx

-- | Env
type Env = F.SEnv Type

extendEnv :: F.Symbol -> Type -> Refining a -> Refining a
extendEnv x ty = locally senv (F.insertSEnv x ty)

lookupEnv :: F.Symbol -> Refining Type
lookupEnv x =
  asks (^. senv . to (F.lookupSEnv x)) >>= \case
    Nothing ->
      throwRefiningError $
        "Can't find variable's refinement type in environment: "
          <> show x
    Just ty -> return ty
