module Flex.Refining.Embedding where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq
import Control.Exception
import Control.Monad (foldM, void, when)
import Data.Bifunctor (second)
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import Data.Typeable
import Flex.Refining.CG
import Flex.Refining.Syntax
import Flex.Syntax (Id, Literal, ModuleId)
import qualified Flex.Syntax as Syn
import GHC.Generics
import GHC.IO.Exception (ExitCode)
import qualified Language.Fixpoint.Horn.Solve as HS
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Misc as F
import qualified Language.Fixpoint.Misc as Misc
import qualified Language.Fixpoint.Parse as FP
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.Config as FC
import qualified Language.Fixpoint.Utils.Files as Files
import System.Exit (exitWith)
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
import Text.Printf (printf)
import Utility

-- | Embedding
--
-- Embed a term as a LF expression
embedTerm :: Term -> CG F.Expr
embedTerm tm = embedPreterm (termPreterm tm)

embedPreterm :: Preterm -> CG F.Expr
embedPreterm = \case
  TermLit lit -> return $ embedLiteral lit
  TermVar x -> return $ embedVar x
  TermBlock block -> error "TODO: embedTerm TermBlock"
  TermApp (AppPrimFun Syn.PrimFunEq) [tm1, tm2] ->
    F.PAtom F.Eq <$> embedTerm tm1 <*> embedTerm tm2
  TermApp (AppPrimFun Syn.PrimFunAnd) [tm1, tm2] ->
    F.PAnd <$> traverse embedTerm [tm1, tm2]
  TermApp (AppPrimFun Syn.PrimFunOr) [tm1, tm2] ->
    F.POr <$> traverse embedTerm [tm1, tm2]
  TermApp (AppPrimFun Syn.PrimFunNot) [tm] ->
    F.PNot <$> embedTerm tm
  TermApp (AppPrimFun pf) args -> throwCG [RefineError $ "invalid primitive function application: " <> show (TermApp (AppPrimFun pf) args)]
  -- TODO: folds in the right direction??
  TermApp (AppVar x) args -> foldM (\e tm -> F.EApp e <$> embedTerm tm) (embedVar x) args

embedVar :: F.Symbol -> F.Expr
embedVar = F.expr

embedLiteral :: Literal -> F.Expr
embedLiteral = \case
  Syn.LiteralInteger n -> F.expr n
  Syn.LiteralFloat x -> error "TODO: how to embed Float?"
  Syn.LiteralBit b -> if b then F.PTrue else F.PFalse
  Syn.LiteralChar c -> F.expr (pack [c])
  Syn.LiteralString txt -> F.expr txt
