module Language.Flex.Refining where

import Control.Lens (locally)
import Control.Monad
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT))
import Data.Foldable (foldrM)
import qualified Data.Map as Map
import qualified Language.Fixpoint.Types as F
import qualified Language.Flex.FlexBug as FlexBug
import Language.Flex.FlexM (FlexM)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Check (checkTerm, runCheckingM)
import Language.Flex.Refining.Query (makeQuery, submitQuery)
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Translating (transTerm, transType)
import qualified Language.Flex.Syntax as Base
import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))

refineModule :: Base.Module Base.Type -> FlexM (Either RefiningError ((), RefiningEnv))
refineModule mdl =
  runExceptT
    ((,) <$> topRefiningCtx mdl <*> topRefiningEnv mdl)
    >>= \case
      Left re -> return . Left $ re
      Right (ctx, env) -> do
        runExceptT $ runReaderT (runStateT (checkModule mdl) env) ctx

checkModule :: Base.Module Base.Type -> RefiningM ()
checkModule Base.Module {..} = do
  forM_ moduleDeclarations checkDeclaration

checkDeclaration :: Base.Declaration Base.Type -> RefiningM ()
checkDeclaration decl = case decl of
  Base.DeclarationFunction Base.Function {..} ->
    foldr (uncurry introTerm) (check label functionBody functionOutput) functionParameters
  Base.DeclarationConstant Base.Constant {..} -> check label constantTerm constantType
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
    -- TODO: introduce fields accessors as uninterpreted functions (?), and assert refinement on each of them
    Base.TypeStructure _struc -> error "introTerm"
    Base.TypeEnum _en -> error "introTerm"
    Base.TypeVariant _vari -> error "introTerm"
    -- TODO: introduce field accessor as uninterpreted function (?), and assert refinemnt on it
    Base.TypeNewtype _new -> error "introTerm"
    -- invalid
    Base.TypeUnifyVar {} -> FlexBug.throw $ FlexM.FlexLog "refining" $ "should not `introTerm` with a unification type varaint during refining" <+> pPrint type_
    -- fallthrough
    _ -> do
      ty <- transType type_
      locally ctxTermVars (Map.insert tmId ty) m

check :: Doc -> Base.Term Base.Type -> Base.Type -> RefiningM ()
check label term type_ = do
  tm <- transTerm term
  ty <- transType type_
  (_, cstr) <- runCheckingM $ checkTerm ty tm
  query <- makeQuery cstr
  result <- submitQuery query
  case result of
    F.Crash mb_stack msg ->
      throwRefiningError $
        vcat
          [ "while checking" <+> label,
            "crash:"
              <+> text msg,
            "stack:" $$ nest 2 (vcat $ (\(re, m_s) -> pPrint re <+> maybe mempty (parens . text) m_s) <$> mb_stack)
          ]
    F.Unsafe _st res ->
      throwRefiningError $
        "unsafe:"
          $$ nest 2 (vcat $ pPrint <$> res)
    F.Safe _st -> return ()