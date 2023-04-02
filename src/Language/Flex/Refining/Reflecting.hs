module Language.Flex.Refining.Reflecting where

import qualified Language.Fixpoint.Types as F
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Prelude (tupleCtorSym)
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import qualified Language.Flex.Syntax as Crude
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility (ticks)

reflTerm :: Term -> RefiningM F.Expr
reflTerm (TermLiteral lit ty) = reflLiteral lit
reflTerm (TermPrimitive prim ty) = reflPrimitive prim
reflTerm (TermLet Nothing _tm1 tm2 _ty) = reflTerm tm2
reflTerm (TermLet (Just x) tm1 tm2 ty) = do
  -- (let x = a in b) ~~> ((lam x => b) a)
  tm1' <- reflTerm tm1
  ty' <- reflType (termType tm1)
  tm2' <- reflTerm tm2
  return $ F.eApps (F.ELam (F.symbol x, ty') tm2') [tm1']
reflTerm (TermAssert _te tm _ty) = reflTerm tm
reflTerm (TermMember te fi _ty) = do
  te' <- reflTerm te
  case termType te of
    TypeNamed tyId -> do
      f <- reflMember tyId fi
      return $ F.eApps f [te']
    ty -> FlexM.throw $ "the type of TermMember's first argument must have a TypeNamed type (with a structure id), but instead it has type:" <+> ticks (pPrint ty)
reflTerm (TermNamed x _) = do
  return $ F.eVar x
reflTerm (TermApplication f tes ty) = do
  tes' <- reflTerm `traverse` tes
  return $ F.eApps (F.eVar f) tes'
reflTerm (TermConstructor ti ti' tes ty) = do
  tes' <- reflTerm `traverse` tes
  return $ F.eApps (F.eVar (ti, ti')) tes'
reflTerm (TermStructure ti tes ty) = do
  tes' <- reflTerm `traverse` tes
  return $ F.eApps (F.eVar ti) tes'
reflTerm (TermMatch te fields ty) = error "!TODO how to reflect match"

reflPrimitive :: Primitive -> RefiningM F.Expr
reflPrimitive (PrimitiveTry te) = error "reflPrimitive PrimitiveTry"
reflPrimitive (PrimitiveTuple te1 te2) = do
  tes' <- reflTerm `traverse` [te1, te2]
  return $ F.eApps (F.eVar tupleCtorSym) tes'
reflPrimitive (PrimitiveArray tes) = do
  tes' <- reflTerm `traverse` tes
  -- F.eApps (F.eVar arrayCtor)
  -- !TODO fold over list structure using the Cons and Nil constructors (in prelude)
  _
reflPrimitive (PrimitiveIf te te' te2) = _wq
reflPrimitive (PrimitiveAnd te te') = _wr
reflPrimitive (PrimitiveOr te te') = _ws
reflPrimitive (PrimitiveNot te) = _wt
reflPrimitive (PrimitiveEq te te') = _wu
reflPrimitive (PrimitiveAdd te te') = _wv
reflPrimitive (PrimitiveExtends te ti) = _ww

reflLiteral :: Crude.Literal -> RefiningM F.Expr
reflLiteral = _

reflType :: Type -> RefiningM F.Sort
reflType = error "reflType"

reflMember :: Crude.TypeId -> Crude.FieldId -> RefiningM F.Expr
reflMember = _
