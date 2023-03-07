module Flex.Refining.Embedding where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq
import Control.Exception
import Control.Lens
import Control.Monad (foldM, void, when)
import Data.Bifunctor (second)
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import Data.Typeable
import Flex.Refining.Refining
import Flex.Refining.Syntax
import Flex.Syntax (Id, Literal, ModuleId)
import qualified Flex.Syntax as Base
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
-- Embedding assumes:
--  - well-refined
--  - no shadowing

-- Embed a term as a LF expression
embedTerm :: Term -> Refining F.Expr
embedTerm tm = embedPreterm (termPreterm tm)

embedPreterm :: Preterm -> Refining F.Expr
embedPreterm = \case
  TermLiteral lit -> return $ embedLiteral lit
  TermVar x -> return $ embedVar x
  TermBlock block -> error "TODO: embedTerm TermBlock: does this require special propogation of constraints about introduced vars??"
  TermApp (AppPrimFun pf) args -> embedAppPrimFun pf args
  -- TODO: folds in the right direction??
  TermApp (AppVar x) args -> foldM (\e tm -> F.EApp e <$> embedTerm tm) (embedVar x) args

embedAppPrimFun :: Base.PrimFun -> [Term] -> Refining F.Expr
embedAppPrimFun pf args = case (pf, args) of
  (Base.PrimFunEq, [tm1, tm2]) ->
    F.PAtom F.Eq <$> embedTerm tm1 <*> embedTerm tm2
  (Base.PrimFunAnd, [tm1, tm2]) ->
    F.PAnd <$> traverse embedTerm [tm1, tm2]
  (Base.PrimFunOr, [tm1, tm2]) ->
    F.POr <$> traverse embedTerm [tm1, tm2]
  (Base.PrimFunNot, [tm]) ->
    F.PNot <$> embedTerm tm
  (pf, args) -> throwRefining [RefineError $ "invalid primitive function application: " <> show (TermApp (AppPrimFun pf) args)]

embedVar :: F.Symbol -> F.Expr
embedVar = F.expr

embedLiteral :: Literal -> F.Expr
embedLiteral = \case
  Base.LiteralInteger n -> F.expr n
  Base.LiteralFloat x -> error "TODO: how to embed Float?"
  Base.LiteralBit b -> if b then F.PTrue else F.PFalse
  Base.LiteralChar c -> F.expr (pack [c])
  Base.LiteralString txt -> F.expr txt
