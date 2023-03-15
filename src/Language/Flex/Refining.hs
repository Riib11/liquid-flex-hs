module Language.Flex.Refining where

import Control.Monad
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import qualified Language.Fixpoint.Types as F
import Language.Flex.FlexM (FlexM)
import Language.Flex.Refining.Check (checkTerm, runCheckingM)
import Language.Flex.Refining.Query (makeQuery, submitQuery)
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Translating (transTerm, transType)
import Language.Flex.Syntax
import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))

refineModule :: Module Type -> RefiningM ()
refineModule Module {..} = do
  forM_ moduleDeclarations refineDeclaration

refineDeclaration :: Declaration Type -> RefiningM ()
refineDeclaration = \case
  DeclarationFunction fun -> refineFunction fun
  DeclarationConstant con -> refineConstant con
  _ -> return ()

refineFunction :: Function Type -> RefiningM ()
refineFunction _fun = error "refineFunction"

refineConstant :: Constant Type -> RefiningM ()
refineConstant Constant {..} = do
  tm <- transTerm constantTerm
  ty <- transType constantType
  (_, cstr) <- runCheckingM $ checkTerm ty tm
  query <- makeQuery cstr
  result <- submitQuery query
  case result of
    F.Crash mb_stack msg ->
      throwRefiningError $
        vcat
          [ "crash:" <+> text msg,
            "stack:" $$ nest 2 (vcat $ (\(re, m_s) -> pPrint re <+> maybe mempty (parens . text) m_s) <$> mb_stack)
          ]
    F.Unsafe _st res ->
      throwRefiningError $
        "unsafe:"
          $$ nest 2 (vcat $ pPrint <$> res)
    F.Safe _st -> return ()
