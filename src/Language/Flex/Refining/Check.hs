{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use camelCase" #-}
module Language.Flex.Refining.Check where

-- TODO: rename this module to "Refining"

import Control.Lens (at, (^.))
import Control.Monad
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans (lift)
import qualified Control.Monad.Writer as Writer
import qualified Control.Monad.Writer.Class as Writer
import Data.Bifunctor (Bifunctor (second))
import Data.Functor
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Types as F
import qualified Language.Flex.FlexBug as FlexBug
import Language.Flex.FlexM (FlexM)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Constraint
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import Language.Flex.Refining.Translating (embedTerm, embedType, eqPred, transType, tupleTypeReft)
import Language.Flex.Refining.Types
import Language.Flex.Syntax (Literal (..))
import qualified Language.Flex.Syntax as Base
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), nest, parens, render, text, vcat, ($$), (<+>))
import Utility (for, ticks)

type CheckingM = Writer.WriterT CstrMonoid RefiningM

runCheckingM :: CheckingM a -> RefiningM (a, Cstr)
runCheckingM = fmap (second (\(CstrMonoid cs) -> cs)) . Writer.runWriterT

liftFlexM_CheckingM :: FlexM a -> CheckingM a
liftFlexM_CheckingM = lift . lift . lift . lift

newtype CstrMonoid = CstrMonoid Cstr

mapCstrMonoid :: (Cstr -> Cstr) -> CstrMonoid -> CstrMonoid
mapCstrMonoid f (CstrMonoid c) = CstrMonoid (f c)

tellCstr :: Cstr -> CheckingM ()
tellCstr = Writer.tell . CstrMonoid

instance Semigroup CstrMonoid where
  CstrMonoid cstr1 <> CstrMonoid cstr2 = CstrMonoid (andCstr cstr1 cstr2)

instance Monoid CstrMonoid where
  mempty = CstrMonoid trivialCstr

-- ** Checking

synthCheckTerm :: TypeReft -> Term Base.Type -> CheckingM (Term TypeReft)
synthCheckTerm tyExpect tm = do
  tm' <- synthTerm tm
  tySynth <- inferTerm tm'
  FlexM.debug . FlexM.FlexLog "refining" $
    "[synthCheckTerm]"
      $$ (nest 2 . vcat)
        [ text "     tm  =" <+> pPrint tm,
          text "     tm' =" <+> pPrint tm',
          text " tySynth =" <+> pPrint tySynth,
          text "tyExpect =" <+> pPrint tyExpect
        ]
  checkSubtype tm' tySynth tyExpect
  return tm'

-- ** Synthesizing

synthTerm :: Term Base.Type -> CheckingM (Term TypeReft)
synthTerm term = case term of
  TermNeutral symId args ty -> do
    args' <- synthTerm `traverse` args
    -- TODO: for transforms, input values can't affect output refinement
    -- type, BUT, newtype/variant/enum constructors should have their args
    -- reflected in their type via `C1(a, b, c) : { X : C | X = C1(a, b, c)
    -- }`
    ty' <- liftFlexM_CheckingM $ transType ty
    return $ TermNeutral symId args' ty'
  TermLiteral lit ty -> do
    -- literals are reflected
    ty' <- liftFlexM_CheckingM $ transType ty
    tm' <-
      mapM_termAnn (mapM_typeAnn (reflectLiteralInReft (void ty') lit)) $
        TermLiteral lit ty'
    return tm'
  TermPrimitive prim ty ->
    synthPrimitive term ty prim
  TermAssert tm1 tm2 _ty -> do
    FlexM.debug . FlexM.FlexLog "refining" $ "[synthTerm]" <+> pPrint term
    -- check asserted term against refinement type { x | x == true }
    ty1 <-
      TypeAtomic TypeBit
        <$> reflectLiteralInReft (typeBit ()) (LiteralBit True) F.trueReft
    tm1' <- synthCheckTerm ty1 tm1
    tm2' <- synthTerm tm2
    ty' <- inferTerm tm2'
    return $ TermAssert tm1' tm2' ty'
  -- let-bindings introduce the following info into context:
  --  - map the Base.TermId to a fresh SymId via introSymId
  --  - map the SymId to an ApplicantType via introApplicantType
  --  - map the SymId to a Term TypeReft via introBinding
  TermLet symId tm bod ty -> do
    tm' <- synthTerm tm

    bod' <- do
      -- p: symId == tm'
      p <- liftFlexM_CheckingM $ eqPred (termVar symId (void $ termAnn tm')) (void <$> tm')
      -- the constraint yielded by checking the body must be wrapped in a
      -- quantification over the binding introduced by the let
      bSort <- liftFlexM_CheckingM $ embedType $ void $ termAnn tm'
      Writer.censor
        ( mapCstrMonoid $
            H.All
              H.Bind
                { bSym = symIdSymbol symId,
                  bSort,
                  bPred = H.Reft p,
                  bMeta = RefiningError (pPrint term)
                }
        )
        $ synthTerm bod

    ty' <- liftFlexM_CheckingM $ transType ty

    return $ TermLet symId tm' bod' ty'
  TermStructure {..} -> do
    -- reflect as you'd expect

    Base.Structure {..} <- getStructure termStructureId

    fields <-
      forM
        (termFields `zip` structureFields)
        \((fieldId, tmField), (fieldId', tyField)) -> do
          unless (fieldId == fieldId') $ FlexBug.throw $ FlexM.FlexLog "refining" $ "field ids are not in matching order between the structure constructor and its annotated structure type:" <+> pPrint term
          tyField' <- liftFlexM_CheckingM $ transType tyField
          (fieldId,) <$> synthCheckTerm tyField' tmField

    ty <- liftFlexM_CheckingM $ transType termAnn

    let tm =
          TermStructure
            { termStructureId,
              termFields = fields,
              termAnn = ty
            }

    -- TODO: include a constraint that the fields satisfy the structure's user
    -- refinement

    mapM_termAnn (mapM_typeAnn $ reflectTermInReft (void <$> tm)) tm

-- | Note that most primitive operations are reflected in refinement.
synthPrimitive :: Term Base.Type -> Base.Type -> Primitive Base.Type -> CheckingM (Term TypeReft)
synthPrimitive _term ty primitive =
  case primitive of
    PrimitiveTuple (tm1, tm2) -> do
      tm1' <- synthTerm tm1
      ty1 <- inferTerm tm1'
      tm2' <- synthTerm tm2
      ty2 <- inferTerm tm2'
      -- need to use tupleTypeReft instead of transType here because tuples are
      -- polymorphic, so the refinements on the components need to be propogated
      -- outwards
      tyTuple <- liftFlexM_CheckingM $ tupleTypeReft [ty1, ty2]
      return $ TermPrimitive (PrimitiveTuple (tm1', tm2')) tyTuple
    PrimitiveIf tm1 tm2 tm3 -> go3 PrimitiveIf tm1 tm2 tm3
    PrimitiveAnd tm1 tm2 -> go2 PrimitiveAnd tm1 tm2
    PrimitiveOr tm1 tm2 -> go2 PrimitiveOr tm1 tm2
    PrimitiveNot tm -> go1 PrimitiveNot tm
    PrimitiveEq tm1 tm2 -> go2 PrimitiveEq tm1 tm2
    PrimitiveAdd tm1 tm2 -> go2 PrimitiveAdd tm1 tm2
    PrimitiveTry _tm -> error "synthPrimitive: PrimitiveType"
    PrimitiveArray _tms -> error "synthPrimitive: PrimitiveArray"
  where
    go2 constr tm1 tm2 = do
      tm1' <- synthTerm tm1
      tm2' <- synthTerm tm2
      let prim = constr tm1' tm2'
      ty' <- liftFlexM_CheckingM $ transType ty
      mapM_termAnn
        (mapM_typeAnn $ reflectPrimitiveInReft (void ty') (void <$> prim))
        $ TermPrimitive prim ty'

    go1 constr tm = do
      tm' <- synthTerm tm
      let prim = constr tm'
      ty' <- liftFlexM_CheckingM $ transType ty
      mapM_termAnn
        (mapM_typeAnn $ reflectPrimitiveInReft (void ty') (void <$> prim))
        $ TermPrimitive prim ty'

    go3 constr tm1 tm2 tm3 = do
      tm1' <- synthTerm tm1
      tm2' <- synthTerm tm2
      tm3' <- synthTerm tm3
      let prim = constr tm1' tm2' tm3'
      ty' <- liftFlexM_CheckingM $ transType ty
      mapM_termAnn
        (mapM_typeAnn $ reflectPrimitiveInReft (void ty') (void <$> prim))
        $ TermPrimitive prim ty'

-- ** Reflection

-- | Given a term and a refinement type over that term's type, include in the
-- refinement that the value is equal to the (embedded) term.
--
-- > reflectTermInReft v { x: a | r } = { x: a | x == v && r }
reflectTermInReft :: Term (Type ()) -> F.Reft -> CheckingM F.Reft
reflectTermInReft tm r = do
  let sort = void $ termAnn tm
  let x = F.reftBind r
  let p = F.reftPred r
  pRefl <-
    liftFlexM_CheckingM . embedTerm $
      TermPrimitive
        (PrimitiveEq (termVar (fromSymbolToSymId x) sort) tm)
        (typeBit ())
  return $ F.reft x (F.conj [pRefl, p])

reflectLiteralInReft :: Type () -> Literal -> F.Reft -> CheckingM F.Reft
reflectLiteralInReft ty lit = reflectTermInReft (TermLiteral lit ty)

reflectPrimitiveInReft :: Type () -> Primitive (Type ()) -> F.Reft -> CheckingM F.Reft
reflectPrimitiveInReft ty prim = reflectTermInReft (TermPrimitive prim ty)

-- ** Inferring

inferTerm :: Term TypeReft -> CheckingM TypeReft
inferTerm = return . termAnn

-- ** Subtyping

checkSubtype :: Term TypeReft -> TypeReft -> TypeReft -> CheckingM ()
checkSubtype tmSynth tySynth tyExpect = do
  FlexM.debug $
    FlexM.FlexLog
      "refining"
      ( "[checkSubType]"
          $$ (nest 2 . vcat)
            [ pPrint tmSynth,
              nest 2 $ " :" <+> pPrint tySynth,
              nest 2 $ "<:" <+> pPrint tyExpect
            ]
      )
  --    forall x : T, p x ==> (p' x')[x' := x]
  --  ----------------------------------------------
  --    {x : T | p x} <: {x' : T | p' y'}
  tellCstr
    =<< ( liftFlexM_CheckingM $
            cstrForall xSynth tySynth $
              cstrHead
                tmSynth
                eSynth
                tyExpect
                (subst eExpect xExpect xSynth)
        )
  where
    rSynth = typeAnn tySynth
    rExpect = typeAnn tyExpect
    (xSynth, eSynth) = (F.reftBind rSynth, F.reftPred rSynth)
    (xExpect, eExpect) = (F.reftBind rExpect, F.reftPred rExpect)
