{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Language.Flex.Refining.Check where

-- TODO: rename this module to "Refining"
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT), void)
import Data.Bifunctor (Bifunctor (second))
import Data.Functor
import qualified Language.Fixpoint.Types as F
import qualified Language.Flex.FlexBug as FlexBug
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Constraint
import Language.Flex.Refining.Embedding (embedTerm)
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import Language.Flex.Refining.Translating (transType, typeTuple)
import Language.Flex.Refining.Types
import Language.Flex.Syntax (Literal (..))
import qualified Language.Flex.Syntax as Base
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), nest, parens, render, text, vcat, ($$), (<+>))
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

synthCheckTerm :: Type -> Term Base.Type -> CheckingM (Term Type)
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

synthTerm :: Term Base.Type -> CheckingM (Term Type)
synthTerm term = case term of
  TermNeutral app args ty -> do
    args' <- synthTerm `traverse` args
    -- TODO: for transforms, input values can't affect output refinement type,
    -- BUT, newtype/variant/enum constructors should have their args reflected
    -- in their type via `C1(a, b, c) : { X : C | X = C1(a, b, c) }
    ty' <- lift $ transType ty
    return $ TermNeutral app args' ty'
  TermLiteral lit ty -> do
    -- literals are reflected
    ty' <- lift $ transType ty
    tm' <-
      mapMTermTopR (mapMTypeTopR (reflectLiteralInReft ty' lit)) $
        TermLiteral lit ty'
    return tm'
  TermPrimitive prim ty ->
    synthPrimitive term ty prim
  TermAssert tm1 tm2 _ty -> do
    FlexM.debug . FlexM.FlexLog "refining" $ "[synthTerm]" <+> pPrint term
    -- check asserted term against refinement type { x | x == true }
    ty1 <-
      TypeAtomic TypeBit
        <$> reflectLiteralInReft (typeBit F.trueReft) (LiteralBit True) F.trueReft
    tm1' <- synthCheckTerm ty1 tm1
    tm2' <- synthTerm tm2
    ty' <- inferTerm tm2'
    return $ TermAssert tm1' tm2' ty'
  TermLet id' tm bod ty -> do
    tm' <- synthTerm tm
    bod' <-
      introId' id' $
        introApplicantType id' (Base.ApplicantType $ getTermTopR tm') $
          synthTerm bod
    ty' <- lift $ transType ty
    return $ TermLet id' tm' bod' ty'

-- | Note that most primitive operations are reflected in refinement.
synthPrimitive :: Term Base.Type -> Base.Type -> Primitive Base.Type -> CheckingM (Term Type)
synthPrimitive _term ty primitive =
  case primitive of
    PrimitiveTuple tms -> do
      tms' <- synthTerm `traverse` tms
      --- tyComps: ..., { xI: aI | rI(xI) }, ...
      tyComps <- inferTerm `traverse` tms'
      tyTuple <- lift $ typeTuple tyComps
      return $ TermPrimitive (PrimitiveTuple tms') tyTuple
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
      ty' <- lift $ transType ty
      mapMTermTopR (mapMTypeTopR (reflectPrimitiveInReft ty' prim)) $
        TermPrimitive prim ty'

    go1 constr tm = do
      tm' <- synthTerm tm
      let prim = constr tm'
      ty' <- lift $ transType ty
      mapMTermTopR (mapMTypeTopR (reflectPrimitiveInReft ty' prim)) $
        TermPrimitive prim ty'

    go3 constr tm1 tm2 tm3 = do
      tm1' <- synthTerm tm1
      tm2' <- synthTerm tm2
      tm3' <- synthTerm tm3
      let prim = constr tm1' tm2' tm3'
      ty' <- lift $ transType ty
      mapMTermTopR (mapMTypeTopR (reflectPrimitiveInReft ty' prim)) $
        TermPrimitive prim ty'

-- ** Reflection

-- | Given a term and a refinement type over that term's type, include in the
-- refinement that the value is equal to the (embedded) term.
--
-- > reflectTermInReft v { x: a | r } = { x: a | x == v && r }
reflectTermInReft :: Term Type -> F.Reft -> CheckingM F.Reft
reflectTermInReft tm r = do
  let sort = getTermTopR tm
  let x = F.reftBind r
  let p = F.reftPred r
  pRefl <-
    lift . embedTerm $
      TermPrimitive
        (PrimitiveEq (termVar (fromSymbolToId' x) sort) tm)
        (typeBit mempty)
  return $ F.reft x (F.conj [pRefl, p])

reflectLiteralInReft :: Type -> Literal -> F.Reft -> CheckingM F.Reft
reflectLiteralInReft ty lit = reflectTermInReft (TermLiteral lit ty)

reflectPrimitiveInReft :: Type -> Primitive Type -> F.Reft -> CheckingM F.Reft
reflectPrimitiveInReft ty prim = reflectTermInReft (TermPrimitive prim ty)

-- ** Inferring

inferTerm :: Term Type -> CheckingM Type
inferTerm = return . getTermTopR

-- ** Subtyping

checkSubtype :: Term Type -> Type -> Type -> CheckingM ()
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
  tellCstr $
    cstrForall xSynth tySynth $
      cstrHead
        tmSynth
        eSynth
        tyExpect
        (subst eExpect xExpect xSynth)
  where
    rSynth = getTypeTopR tySynth
    rExpect = getTypeTopR tyExpect
    (xSynth, eSynth) = (F.reftBind rSynth, F.reftPred rSynth)
    (xExpect, eExpect) = (F.reftBind rExpect, F.reftPred rExpect)
