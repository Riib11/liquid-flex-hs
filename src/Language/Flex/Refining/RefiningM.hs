{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Language.Flex.Refining.RefiningM where

import Control.DeepSeq (NFData)
import Control.Lens
import Control.Monad
import Control.Monad.Except (ExceptT, MonadError (throwError), MonadTrans (lift))
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.State (StateT, gets, modify)
import qualified Data.Map as Map
import GHC.Generics
import qualified Language.Fixpoint.Parse as FP
import qualified Language.Fixpoint.Types as F
import qualified Language.Flex.FlexBug as FlexBug
import Language.Flex.FlexM (FlexLog (FlexLog), FlexM, pprintInline)
import Language.Flex.Refining.Syntax
import qualified Language.Flex.Syntax as Base
import Text.PrettyPrint.HughesPJ (Doc, nest, render, text, ($$), (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))

-- ** RefiningM

type RefiningM = StateT RefiningEnv (ReaderT RefiningCtx (ExceptT RefiningError FlexM))

liftFlexM :: FlexM a -> RefiningM a
liftFlexM = lift . lift . lift

newtype RefiningError = RefiningError Doc
  deriving (Generic, Show)

instance NFData RefiningError

instance Pretty RefiningError where
  pPrint (RefiningError msg) = msg

instance F.Fixpoint RefiningError where
  toFix = pPrint

instance F.Loc RefiningError where
  srcSpan _ = F.dummySpan

instance F.PPrint RefiningError where
  pprintTidy _ = text . render . pPrint

throwRefiningError :: Doc -> RefiningM a
throwRefiningError msg = throwError $ RefiningError msg

-- TODO: refined structures and newtypes
data RefiningCtx = RefiningCtx
  { _ctxTypings :: Map.Map Base.TermId TypeReft,
    _ctxId's :: Map.Map (Base.Applicant ()) Id',
    _ctxApplicants :: Map.Map Id' (Base.ApplicantType TypeReft),
    _ctxFunctions :: Map.Map Id' (Base.Function Base.Type),
    _ctxBindings :: Map.Map Id' (Term TypeReft)
  }

data RefiningEnv = RefiningEnv
  { _freshSymbolIndex :: Int
  }

makeLenses ''RefiningCtx
makeLenses ''RefiningEnv

topRefiningCtx :: Base.Module Base.Type -> ExceptT RefiningError FlexM RefiningCtx
topRefiningCtx _mdl = do
  return
    RefiningCtx
      { _ctxTypings = mempty,
        _ctxId's = mempty,
        _ctxApplicants = mempty,
        _ctxFunctions = mempty
      }

topRefiningEnv :: Base.Module Base.Type -> ExceptT RefiningError FlexM RefiningEnv
topRefiningEnv _mdl = do
  return
    RefiningEnv
      { _freshSymbolIndex = 0
      }

-- ** Utilities

lookupTyping tmId =
  asks (^. ctxTypings . at tmId)
    >>= \case
      Nothing -> FlexBug.throw $ FlexLog "refining" $ "unknown:" <+> pPrint tmId
      Just ty -> return ty

lookupId' app =
  asks (^. ctxId's . at app) >>= \case
    Nothing -> FlexBug.throw $ FlexLog "refining" $ "unknown:" <+> pPrint app
    Just id' -> return id'

introId' id' = case id'MaybeTermId id' of
  Nothing -> id
  Just tmId ->
    locally
      (ctxId's . at (Base.termIdApplicant tmId (Base.ApplicantType ())))
      (const $ Just id')

lookupApplicantType id' =
  asks (^. ctxApplicants . at id') >>= \case
    Nothing -> FlexBug.throw $ FlexLog "refining" $ "unknown applicant id:" <+> pPrint id'
    Just appTy -> return (Right <$> appTy)

introApplicantType id' appTy =
  locally
    (ctxApplicants . at id')
    (const $ Just appTy)

lookupFunction id' =
  asks (^. ctxFunctions . at id') >>= \case
    Nothing -> FlexBug.throw $ FlexLog "refining" $ "unknown function id:" <+> pPrint id'
    Just func -> return func

introBinding id' tm =
  locally
    (ctxBindings . at id')
    (const $ Just tm)

freshenBind :: F.Reft -> RefiningM F.Reft
freshenBind r = do
  let x = F.reftBind r
  x' <- freshSymbol (render $ pprintInline x)
  return $ F.substa (\y -> if y == x then x' else y) r

freshenTermId :: Base.TermId -> RefiningM Base.TermId
freshenTermId = error "TODO: freshenTermId"

freshSymbol :: String -> RefiningM F.Symbol
freshSymbol str = do
  i <- gets (^. freshSymbolIndex)
  modifying freshSymbolIndex (1 +)
  return $ F.symbol (str <> "#" <> show i)

freshId' :: String -> RefiningM Id'
freshId' str = do
  sym <- freshSymbol str
  return $ fromSymbolToId' sym

freshId'TermId :: Base.TermId -> RefiningM Id'
freshId'TermId tmId = do
  id'Symbol <- freshSymbol (render . pPrint $ tmId)
  return Id' {id'Symbol, id'MaybeTermId = Just tmId}

parsePred :: String -> F.Pred
parsePred = FP.doParse' FP.predP "parsePred"