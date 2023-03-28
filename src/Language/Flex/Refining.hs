{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wpartial-type-signatures #-}

module Language.Flex.Refining where

import Control.Lens
import Control.Monad
import Control.Monad.Except (ExceptT, MonadTrans (lift), runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT))
import Data.Bifunctor (first)
import Data.Foldable (foldrM)
import qualified Data.Map as Map
import qualified Language.Fixpoint.Types as F
import Language.Flex.FlexM (FlexM)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Check (runCheckingM, synthCheckTerm)
import Language.Flex.Refining.Query (makeQuery, submitQuery)
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax (RefiningError)
import Language.Flex.Refining.Translating (transRefinedTypeRefinement, transTerm, transType)
import qualified Language.Flex.Syntax as Base
import Text.PrettyPrint.HughesPJ hiding (first)
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility

refineModule :: Base.Module Base.Type -> FlexM (Either RefiningError ((), RefiningEnv))
refineModule mdl = FlexM.markSection [FlexM.FlexMarkStep "refineModule" Nothing] do
  runExceptT
    ((,) <$> topRefiningCtx mdl <*> topRefiningEnv mdl)
    >>= \case
      Left err -> return . Left $ err
      Right (ctx, env) -> do
        runExceptT $ runReaderT (runStateT (checkModule mdl) env) ctx

checkModule :: Base.Module Base.Type -> RefiningM ()
checkModule Base.Module {..} = do
  -- introduce refined translations into ctxRefinedTypes'
  localM
    ( \ctx -> do
        foldM
          ( \ctx' Base.RefinedType {..} -> do
              Base.Structure {..} <- getStructure refinedTypeId
              tm <-
                transRefinedTypeRefinement
                  -- give structure fields as local variables
                  (first Base.fromFieldIdToTermId <$> structureFields)
                  refinedTypeRefinement
              return $ ctx' & ctxRefinedTypes' . at refinedTypeId ?~ tm
          )
          ctx
          (ctx ^. ctxRefinedTypes)
    )
    (forM_ moduleDeclarations checkDeclaration)

checkDeclaration :: Base.Declaration Base.Type -> RefiningM ()
checkDeclaration decl = FlexM.markSection [FlexM.FlexMarkStep "checkDeclaration" . Just $ pPrint decl] do
  case decl of
    Base.DeclarationFunction Base.Function {..} -> do
      for functionParameters (check label functionBody functionOutput) $
        uncurry introTerm
    Base.DeclarationConstant Base.Constant {..} -> do
      check label constantBody constantType
    _ -> return ()
  where
    label = pPrint decl

introTerm :: Base.TermId -> Base.Type -> RefiningM a -> RefiningM a
introTerm tmId type_ m = do
  case type_ of
    Base.TypeArray _ty -> error "introTerm"
    Base.TypeTuple _tys -> error "introTerm"
    Base.TypeOptional _ty -> error "introTerm"
    Base.TypeNamed _ti -> error "introTerm"
    -- intro assumption that fields satisfy structure refinement
    Base.TypeStructure _struc -> error "introTerm"
    Base.TypeEnum _en -> error "introTerm"
    Base.TypeVariant _vari -> error "introTerm"
    -- intro assumption that fields satisfy newtype refinement
    Base.TypeNewtype _new -> error "introTerm"
    -- invalid
    Base.TypeUnifyVar {} -> FlexM.throw $ "should not `introTerm` with a unification type varaint during refining" <+> pPrint type_
    -- fallthrough
    _ -> do
      ty <- FlexM.liftFlex $ transType type_
      locally ctxTypings (Map.insert tmId ty) m

check :: Doc -> Base.Term Base.Type -> Base.Type -> RefiningM ()
check label term type_ = FlexM.markSection [FlexM.FlexMarkStep "check" . Just $ pPrint term <+> ":?" <+> pPrint type_] do
  tm <- transTerm term
  $(FlexM.debugThing False [|pPrint|] [|tm|])
  ty <- FlexM.liftFlex $ transType type_
  $(FlexM.debugThing False [|pPrint|] [|ty|])
  (_, cstr) <- runCheckingM $ synthCheckTerm ty tm
  query <- makeQuery cstr
  result <- submitQuery query
  case result of
    F.Crash mb_stack msg ->
      FlexM.throw $
        vcat
          [ "while checking" $$ nest 2 label,
            "crash:"
              <+> text msg,
            "stack:" $$ nest 2 (vcat $ (\(err, m_s) -> pPrint err <+> maybe mempty (parens . text) m_s) <$> mb_stack)
          ]
    F.Unsafe _st res ->
      throwRefiningError $
        "unsafe:"
          $$ nest 2 (vcat $ pPrint <$> res)
    F.Safe _st -> return ()