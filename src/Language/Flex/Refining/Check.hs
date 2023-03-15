module Language.Flex.Refining.Check where

import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT), void)
import Data.Bifunctor (Bifunctor (second))
import qualified Language.Fixpoint.Types as F
import qualified Language.Flex.FlexBug as FlexBug
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Constraint
import Language.Flex.Refining.Embedding (embedTerm)
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import Language.Flex.Refining.Translating (transType)
import Language.Flex.Refining.Types
import Language.Flex.Syntax (Literal (..))
import qualified Language.Flex.Syntax as Base
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), parens, render, text, (<+>))
import Utility (ticks)

type CheckingM = WriterT CstrMonoid RefiningM

runCheckingM :: CheckingM a -> RefiningM (a, Cstr)
runCheckingM = fmap (second (\(CstrMonoid cs) -> cs)) . runWriterT

newtype CstrMonoid = CstrMonoid Cstr

tellCstr :: Cstr -> CheckingM ()
tellCstr = tell . CstrMonoid

instance Semigroup CstrMonoid where
  CstrMonoid cstr1 <> CstrMonoid cstr2 = CstrMonoid (andCstr cstr1 cstr2)

instance Monoid CstrMonoid where
  mempty = CstrMonoid trivialCstr

-- ** Checking

checkTerm :: Type -> Term Base.Type -> CheckingM (Term Type)
checkTerm tyExpect tm = do
  tm' <- synthTerm tm
  tySynth <- inferTerm tm'
  checkSubtype tySynth tyExpect
  return tm'

-- ** Synthesizing

synthTerm :: Term Base.Type -> CheckingM (Term Type)
synthTerm term = case term of
  TermNamed tmId _ ->
    TermNamed tmId <$> lift (lookupTermId tmId)
  TermLiteral lit ty -> do
    -- literals are reflected
    ty' <- lift $ transType ty
    -- lift $ FlexM.tell $ FlexM.FlexLog "refining" $ "synthTerm, TermLiteral, lit =" <+> pPrint lit
    tm' <-
      mapMTermTopR (mapMTypeTopR (reflectInReft term)) $
        TermLiteral lit ty'
    -- lift $ FlexM.tell $ FlexM.FlexLog "refining" $ "synthTerm, TermLiteral, tm' =" <+> text (show tm')
    return tm'
  TermPrimitive prim ty ->
    synthPrimitive term ty prim
  TermAssert tm1 tm2 _ty -> do
    -- check asserted term against refinement type { x | x == true }
    ty1 <-
      TypeAtomic TypeBit
        <$> reflectInReft (TermLiteral (LiteralBit True) Base.TypeBit) F.trueReft
    tm1' <- checkTerm ty1 tm1
    tm2' <- synthTerm tm2
    ty' <- inferTerm tm2'
    return $ TermAssert tm1' tm2' ty'
  -- invalid
  TermSymbol _ _ -> FlexBug.throw $ FlexM.FlexLog "refining" "should never try to synthesize type of symbol term, since it's only introduced in refinements which are immediately embedded"

-- most primitive operations are reflected in refinement
synthPrimitive :: Term Base.Type -> Base.Type -> Primitive Base.Type -> CheckingM (Term Type)
synthPrimitive term ty prim =
  case prim of
    PrimitiveTry _tm -> error "synthPrimitive: PrimitiveTry"
    PrimitiveTuple _tms -> error "synthPrimitive: PrimitiveTuple"
    PrimitiveArray _tms -> error "synthPrimitive: PrimitiveArray"
    PrimitiveIf tm1 tm2 tm3 -> go3 PrimitiveIf tm1 tm2 tm3
    PrimitiveAnd tm1 tm2 -> go2 PrimitiveAnd tm1 tm2
    PrimitiveOr tm1 tm2 -> go2 PrimitiveOr tm1 tm2
    PrimitiveNot tm -> go1 PrimitiveNot tm
    PrimitiveEq tm1 tm2 -> go2 PrimitiveEq tm1 tm2
    PrimitiveAdd tm1 tm2 -> go2 PrimitiveAdd tm1 tm2
  where
    go2 constr tm1 tm2 = do
      tm1' <- synthTerm tm1
      tm2' <- synthTerm tm2
      ty' <- lift $ transType ty
      mapMTermTopR (mapMTypeTopR (reflectInReft term)) $
        TermPrimitive (constr tm1' tm2') ty'

    go1 constr tm = do
      tm' <- synthTerm tm
      ty' <- lift $ transType ty
      mapMTermTopR (mapMTypeTopR (reflectInReft term)) $
        TermPrimitive (constr tm') ty'

    go3 constr tm1 tm2 tm3 = do
      tm1' <- synthTerm tm1
      tm2' <- synthTerm tm2
      tm3' <- synthTerm tm3
      ty' <- lift $ transType ty
      mapMTermTopR (mapMTypeTopR (reflectInReft term)) $
        TermPrimitive (constr tm1' tm2' tm3') ty'

-- ** Reflection

-- | predReflect tm { x | r } = { x | x == tm && r }
reflectInReft :: Term r -> F.Reft -> CheckingM F.Reft
reflectInReft tm r = do
  let x = F.reftBind r
  let p = F.reftPred r
  pRefl <- lift $ embedTerm (TermPrimitive (PrimitiveEq (TermSymbol x ()) (void tm)) ())
  return $ F.reft x (F.conj [pRefl, p])

-- ** Inferring

inferTerm :: Term Type -> CheckingM Type
inferTerm = \case
  TermNamed _ ty -> return ty
  TermLiteral _ ty -> return ty
  TermPrimitive _ ty -> return ty
  TermAssert _ _ ty -> return ty
  -- invalid
  TermSymbol _ _ -> FlexBug.throw $ FlexM.FlexLog "refining" "should never try to infer type of symbol term, since it's only introduced in refinements which are immediately embedded"

-- ** Subtyping

checkSubtype :: Type -> Type -> CheckingM ()
checkSubtype tySynth tyExpect = case (tySynth, tyExpect) of
  (TypeAtomic at1 r1, TypeAtomic at2 r2)
    | at1 == at2 -> do
        lift $ FlexM.tell $ FlexM.FlexLog "refining" ("[checkSubType]" <+> pPrint tySynth <+> text "<:" <+> pPrint tyExpect)
        --    forall x : T, p x ==> (p' x')[x' := x]
        --  ----------------------------------------------
        --    {x : T | p x} <: {x' : T | p' y'}
        tellCstr $
          forallCstr x1 tySynth $
            headCstr (subst e2 x2 x1)
    where
      (x1, _e1) = (F.reftBind r1, F.reftPred r1)
      (x2, e2) = (F.reftBind r2, F.reftPred r2)
  _ -> lift $ throwRefiningError $ "the type" <+> ticks (pPrint tySynth) <+> "cannot be a subtype of the type" <+> ticks (pPrint tyExpect)
