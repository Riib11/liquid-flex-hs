{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# HLINT ignore "Use camelCase" #-}

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
import Language.Flex.FlexM (FlexLog (FlexLog), FlexM, MonadFlex, freshSymbol, liftFlex)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Syntax
import qualified Language.Flex.Syntax as Base
import Text.PrettyPrint.HughesPJ (Doc, nest, render, text, ($$), (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility (comps, foldrM, pprintInline, renderInline)

-- ** RefiningM

type RefiningM = StateT RefiningEnv (ReaderT RefiningCtx (ExceptT RefiningError FlexM))

throwRefiningError :: Doc -> RefiningM a
throwRefiningError msg = throwError $ RefiningError msg

-- TODO: refined structures and newtypes
data RefiningCtx = RefiningCtx
  { _ctxTypings :: Map.Map Base.TermId TypeReft,
    -- During translation: map of each applicant to its corresponding @SymId@,
    -- which is used during embedding
    _ctxSymIds :: Map.Map (Base.Applicant ()) SymId,
    _ctxSymbols :: Map.Map F.Symbol SymId,
    -- | During translation: context of top and local refined signatures
    _ctxApplicants :: Map.Map SymId (Base.ApplicantType TypeReft),
    -- | Before translation: accumulate functions
    _ctxFunctions :: Map.Map SymId (Base.Function Base.Type),
    -- | Before translation: accumulate structures
    _ctxStructures :: Map.Map Base.TypeId Base.Structure,
    -- | Init before translation: accumulate refined types
    _ctxRefinedTypes :: Map.Map Base.TypeId (Base.RefinedType Base.Type),
    -- | During translation, before refinement-checking: translated but not yet
    -- refined refinements on types, since the refinement-checking incurred (in
    -- many different ways) by the refined types is performed inline at each
    -- instance
    _ctxRefinedTypes' :: Map.Map Base.TypeId (Term Base.Type)
  }

data RefiningEnv = RefiningEnv
  {}

makeLenses ''RefiningCtx
makeLenses ''RefiningEnv

topRefiningCtx :: Base.Module Base.Type -> ExceptT RefiningError FlexM RefiningCtx
topRefiningCtx Base.Module {..} = do
  -- TODO: add structures into context
  -- TODO: add newtypes into context
  -- TODO: add variants into context
  -- TODO: add enums into context
  -- TODO: add functions into context
  -- TODO: add transforms into context
  -- TODO: add constants into context
  foldrM
    ( \decl ctx -> do
        case decl of
          Base.DeclarationStructure struct@Base.Structure {..} ->
            -- - TODO: add projectors into context
            -- - TODO: translate TermMember into an application of the projector
            --   instead of reflecting TermMember in the refinement-level syntax
            return $ ctx & ctxStructures %~ Map.insert structureId struct
          Base.DeclarationNewtype _new -> return ctx
          Base.DeclarationVariant _vari -> return ctx
          Base.DeclarationEnum _en -> return ctx
          Base.DeclarationAlias _al -> return ctx
          Base.DeclarationFunction _func -> return ctx
          Base.DeclarationConstant _con -> return ctx
          Base.DeclarationRefinedType reftTy@Base.RefinedType {..} ->
            return $ ctx & ctxRefinedTypes %~ Map.insert refinedTypeId reftTy
    )
    RefiningCtx
      { _ctxTypings = mempty,
        _ctxSymIds = mempty,
        _ctxSymbols = mempty,
        _ctxApplicants = mempty,
        _ctxFunctions = mempty,
        _ctxStructures = mempty,
        _ctxRefinedTypes = mempty,
        _ctxRefinedTypes' = mempty
      }
    moduleDeclarations

topRefiningEnv :: Base.Module Base.Type -> ExceptT RefiningError FlexM RefiningEnv
topRefiningEnv _mdl = do
  return
    RefiningEnv {}

-- ** Utilities

getStructure structId =
  asks (^. ctxStructures . at structId) >>= \case
    Nothing -> FlexM.throw $ "unknown structure:" <+> pPrint structId
    Just struct -> return struct

getRefinedType' structId =
  asks (^. ctxRefinedTypes' . at structId)
    >>= \case
      Nothing -> FlexM.throw $ "unknown refined structure id:" <+> pPrint structId
      Just reftTy -> return reftTy

getTyping tmId =
  asks (^. ctxTypings . at tmId)
    >>= \case
      Nothing -> FlexM.throw $ "unknown:" <+> pPrint tmId
      Just ty -> return ty

getSymId app =
  asks (^. ctxSymIds . at app) >>= \case
    Nothing -> FlexM.throw $ "unknown:" <+> pPrint app
    Just symId -> return symId

-- can only introduce SymIds that have a TermId
-- TODO: bug if try to introduce SymId without a TermId?
introSymId symId =
  comps
    [ case symIdMaybeTermId symId of
        Nothing -> id
        Just tmId ->
          locally
            (ctxSymIds . at (Base.termIdApplicant tmId (Base.ApplicantType ())))
            (const $ Just symId),
      locally
        (ctxSymbols . at (symIdSymbol symId))
        (const $ Just symId)
    ]

introSymbol sym =
  locally
    (ctxSymbols . at sym)
    (const $ Just SymId {symIdSymbol = sym, symIdMaybeTermId = Nothing})

getSymbolSymId sym =
  asks (^. ctxSymbols . at sym) >>= \case
    Nothing -> FlexM.throw $ "unknown symbol:" <+> F.pprint sym
    Just symId -> return symId

getApplicantType symId =
  asks (^. ctxApplicants . at symId) >>= \case
    Nothing -> FlexM.throw $ "unknown applicant id:" <+> pPrint symId
    Just appTy -> return (Right <$> appTy)

introApplicantType symId appTy =
  locally
    (ctxApplicants . at symId)
    (const $ Just appTy)

getFunction symId =
  asks (^. ctxFunctions . at symId) >>= \case
    Nothing -> FlexM.throw $ "unknown function id:" <+> pPrint symId
    Just func -> return func

freshenQReftBind :: MonadFlex m => QReft -> m QReft
freshenQReftBind qr = do
  r <- freshenReftBind (qreftReft qr)
  return qr {qreftReft = r}

freshenReftBind :: MonadFlex m => F.Reft -> m F.Reft
freshenReftBind r = do
  let x = F.reftBind r
  x' <- liftFlex $ freshSymbol (render $ pprintInline x)
  return $ F.substa (\y -> if y == x then x' else y) r

freshenTermId :: Base.TermId -> RefiningM Base.TermId
freshenTermId = error "TODO: freshenTermId"

freshSymId :: String -> RefiningM SymId
freshSymId str = do
  sym <- liftFlex $ freshSymbol str
  return $ fromSymbolToSymId sym

freshSymIdTermId :: Base.TermId -> RefiningM SymId
freshSymIdTermId tmId = do
  symIdSymbol <- liftFlex $ freshSymbol (render . pPrint $ tmId)
  return SymId {symIdSymbol, symIdMaybeTermId = Just tmId}

parsePred :: String -> F.Pred
parsePred = FP.doParse' FP.predP "parsePred"