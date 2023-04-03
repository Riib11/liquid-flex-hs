module Language.Flex.Refining.Checking where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Foldable (traverse_)
import Data.Maybe
import qualified Language.Fixpoint.Horn.Types as H
import Language.Fixpoint.Types as F
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Constraint
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Reflecting
import Language.Flex.Refining.Syntax
import qualified Language.Flex.Syntax as Crude
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility

-- * Checking

-- ** CheckingM

-- type CheckingM a = WriterT CstrQE RefiningM a
type CheckingM a = WriterT [H.Cstr Term] RefiningM a

-- ** Checking

-- | Check all refinement constraints that arise within a term.
checkTerm :: Term -> CheckingM ()
checkTerm (TermLiteral _lit _) =
  return ()
checkTerm (TermPrimitive prim ty) =
  checkPrimitive ty prim
checkTerm (TermLet mb_tmId tm1 tm2 _) = do
  checkTerm tm1
  ( case mb_tmId of
      Nothing -> return id
      Just tmId -> do
        ex1 <- lift $ reflTerm tm1
        return $ mapWriterT $ local (ctxBindings . at tmId ?~ TermExpr tm1 ex1)
    )
    <*> return (checkTerm tm2)
  case mb_tmId of
    Nothing -> do
      checkTerm tm2
    Just tmId -> do
      ex1 <- lift $ reflTerm tm1
      mapWriterT (local $ ctxBindings . at tmId ?~ TermExpr tm1 ex1) $
        checkTerm tm2
checkTerm (TermAssert tm1 tm2 _) = do
  ex1 <- lift $ reflTerm tm1
  -- !TODO check assertion here
  censor
    ( \CstrQE {..} ->
        CstrQE
          { cqeQualifiers,
            cqePred =
              H.PAnd
                [ cqePred,
                  -- H.Head
                  --   (H.Reft ex1)
                  --   (RefiningError $ "unproved assertion: " <+> pPrint tm1)
                  H.Reft ex1
                ],
            cqeTermExprs = cqeTermExprs <> [TermExpr tm1 ex1]
          }
    )
    $ checkTerm tm2
checkTerm (TermMember _tm _fieldId _) =
  error "checkTerm TermMember"
checkTerm (TermNamed _tmId _) =
  return ()
checkTerm (TermApplication _tmId tms _) =
  checkTerm `traverse_` tms
checkTerm (TermConstructor _varntId _ctorId tms _) =
  checkTerm `traverse_` tms
checkTerm (TermStructure _structId fields _) =
  checkTerm `traverse_` fields
checkTerm (TermMatch tm branches _) = do
  forM_ branches (uncurry (checkBranch tm))

-- > matchTerm  = `a`
-- > branchPat  = `C x y z`
-- > branchTerm = `b`
-- > constraint = `forall x y z . (C x y z == a)  ==>  checkTerm b`
-- !TODO how to introduce equality as "local assumption"?
checkBranch :: Term -> Pattern -> Term -> CheckingM ()
checkBranch matchTerm (PatternConstructor tyId ctorId paramIds) branchTerm = do
  trueExpr <- lift $ reflTerm trueTerm
  quals <- forM paramIds \paramId ->
    makeForallQualifier (F.symbol paramId) [] trueExpr
  local
    ( \(CstrQE {..}) ->
        CstrQE
          { cqeQualifiers = cqeQualifiers <> quals,
            cqePred = _,
            cqeTermExprs = _whh7
          }
    )
    $ checkTerm branchTerm

checkPrimitive :: Type -> Primitive -> CheckingM ()
checkPrimitive _type_ (PrimitiveTry tm) = do
  checkTerm tm
checkPrimitive _type_ (PrimitiveTuple tm1 tm2) = do
  checkTerm tm1
  checkTerm tm2
checkPrimitive _type_ (PrimitiveArray tms) = do
  checkTerm `traverse_` tms
checkPrimitive _type_ (PrimitiveIf tm1 tm2 tm3) = do
  checkTerm tm1
  checkTerm tm2
  checkTerm tm3
checkPrimitive _type_ (PrimitiveAnd tm1 tm2) = do
  checkTerm tm1
  checkTerm tm2
checkPrimitive _type_ (PrimitiveOr tm1 tm2) = do
  checkTerm tm1
  checkTerm tm2
checkPrimitive _type_ (PrimitiveNot tm) = do
  checkTerm tm
checkPrimitive _type_ (PrimitiveEq tm1 tm2) = do
  checkTerm tm1
  checkTerm tm2
checkPrimitive _type_ (PrimitiveAdd tm1 tm2) = do
  checkTerm tm1
  checkTerm tm2
checkPrimitive _type_ (PrimitiveExtends tm tyId) = do
  checkTerm tm
