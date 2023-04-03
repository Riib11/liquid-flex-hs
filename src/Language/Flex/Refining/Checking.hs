module Language.Flex.Refining.Checking where

import Data.Maybe (fromMaybe)
import Language.Fixpoint.Types as F
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Reflecting
import Language.Flex.Refining.Syntax
import qualified Language.Flex.Syntax as Crude
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility

type CheckingM = RefiningM

-- | Check all refinement constraints that arise within a term.
checkTerm :: Term -> CheckingM ()
checkTerm (TermLiteral lit ty) = return ()
checkTerm (TermPrimitive prim ty) = error "checkTerm TermPrimitive"
checkTerm (TermLet mb_tmId tm1 tm2 ty) = error "checkTerm TermLet"
checkTerm (TermAssert tm1 tm2 ty) = error "!TODO"
checkTerm (TermMember tm fieldId ty) = error "checkTerm TermMember"
checkTerm (TermNamed tmId ty) = error "checkTerm TermNamed"
checkTerm (TermApplication tmId tms ty) = error "checkTerm TermApplication"
checkTerm (TermConstructor varntId ctorId tms ty) = error "checkTerm TermConstructor"
checkTerm (TermStructure structId fields ty) = error "checkTerm TermStructure"
checkTerm (TermMatch tm branches ty) = error "checkTerm TermMatch"
