{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# HLINT ignore "Use camelCase" #-}

module Language.Flex.Refining.RefiningM where

import Control.DeepSeq (NFData)
import Control.Lens
import Control.Monad
import Control.Monad.Except (ExceptT, MonadError (throwError), MonadTrans (lift))
import Control.Monad.Reader (MonadReader (ask), ReaderT, asks, runReader)
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
import Utility (comps, foldrM, pprintInline, renderInline, secondM)

-- ** RefiningM

type RefiningM = StateT RefiningEnv (ReaderT RefiningCtx (ExceptT RefiningError FlexM))

throwRefiningError :: Doc -> RefiningM a
throwRefiningError msg = throwError $ RefiningError msg

-- TODO: refined structures and newtypes
data RefiningCtx = RefiningCtx
  { -- During translation: map of each applicant to its corresponding @SymId@,
    -- which is used during embedding
    _ctxSymIds :: Map.Map (Base.Applicant ()) SymId,
    _ctxSymbols :: Map.Map F.Symbol SymId,
    -- | During translation: context of top and local refined signatures
    _ctxApplicantTypes :: Map.Map SymId (Base.ApplicantType TypeReft),
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
    _ctxRefinedTypes' :: Map.Map Base.TypeId (Term Base.Type),
    -- | Before translation: accululate variants
    _ctxVariants :: Map.Map Base.TypeId (Base.Variant Base.Type)
  }

data RefiningEnv = RefiningEnv
  {}

makeLenses ''RefiningCtx
makeLenses ''RefiningEnv

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
  asks (^. ctxApplicantTypes . at symId) >>= \case
    Nothing -> FlexM.throw $ "unknown applicant id:" <+> pPrint symId
    Just appTy -> return (Right <$> appTy)

introApplicantType symId appTy =
  locally
    (ctxApplicantTypes . at symId)
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

freshSymId :: MonadFlex m => String -> m SymId
freshSymId str = do
  sym <- freshSymbol str
  return $ fromSymbolToSymId sym

freshSymIdTermId :: MonadFlex m => Base.TermId -> m SymId
freshSymIdTermId tmId = do
  symIdSymbol <- freshSymbol (render . pPrint $ tmId)
  return SymId {symIdSymbol, symIdMaybeTermId = Just tmId}

parsePred :: String -> F.Pred
parsePred = FP.doParse' FP.predP "parsePred"