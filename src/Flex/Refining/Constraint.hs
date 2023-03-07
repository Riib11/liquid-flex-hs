module Flex.Refining.Constraint where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq
import Control.Exception
import Control.Monad (foldM, void, when)
import Data.Bifunctor (second)
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import Data.Typeable
import Flex.Refining.Refining
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

-- | Constraints
--
-- In Liquid Fixpoint, `H.Cstr` has the following form:
--
--    data Cstr a
--      = Head  !Pred !a                  -- ^ p
--      | CAnd  ![Cstr a]                 -- ^ c1 /\ ... /\ cn
--      | All   !(Bind a)  !(Cstr a)      -- ^ \all x:t. p => c
--      | Any   !(Bind a)  !(Cstr a)      -- ^ \exi x:t. p /\ c or is it \exi x:t. p => c?
--      deriving (Data, Typeable, Generic, Functor, Eq)
type Cstr = H.Cstr RefineError

trivialCstr :: Cstr
trivialCstr = H.CAnd []

andCstr :: Cstr -> Cstr -> Cstr
andCstr cstr1 cstr2 = andCstrs [cstr1, cstr2]

andCstrs :: [Cstr] -> Cstr
andCstrs = H.CAnd

forallCstr :: F.Symbol -> BaseType -> Cstr -> Cstr
forallCstr x ty cstr = case sortPred x ty of
  Just (srt, prd) -> H.All (H.Bind x srt prd (RefineError "forallCstr")) cstr
  _ -> cstr

-- subtyping constraint (??)
headCstr :: F.Expr -> Cstr
headCstr e = H.Head (H.Reft e) (RefineError $ "Subtype error: " <> show e)

reftSymbol :: F.Reft -> F.Symbol
reftSymbol (F.Reft (x, _)) = x

reftExpr :: F.Reft -> F.Expr
reftExpr (F.Reft (_, e)) = e

sortPred :: F.Symbol -> BaseType -> Maybe (F.Sort, H.Pred)
sortPred x = \case
  TypeAtomic r atom ->
    Just
      ( case atom of
          AtomicInt -> F.intSort
          AtomicFloat -> F.realSort
          AtomicBit -> F.boolSort
          AtomicChar -> F.charSort
          AtomicString -> F.strSort,
        mkReft r
      )
  where
    mkReft r = H.Reft (subst (reftExpr r) (reftSymbol r) x)
