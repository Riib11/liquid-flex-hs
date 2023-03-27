{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Flex.Refining.Check where

-- TODO: rename this module to "Refining"

import Control.Lens (at, (^.))
import Control.Monad
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans (lift)
import qualified Control.Monad.Writer as Writer
import qualified Control.Monad.Writer.Class as Writer
import Data.Bifunctor
import Data.Bifunctor (Bifunctor (second))
import Data.Functor
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Types as F
import Language.Flex.FlexM (FlexM, markSection)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Constraint
import Language.Flex.Refining.Logic (conjPred)
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import Language.Flex.Refining.Translating (embedTerm, embedType, eqPred, structureSymbol, transTerm, transType, tupleTypeReft)
import Language.Flex.Refining.Types
import Language.Flex.Syntax (Literal (..))
import qualified Language.Flex.Syntax as Base
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), hsep, nest, parens, render, text, vcat, ($$), (<+>))
import Utility (for, ticks)

type CheckingM = Writer.WriterT CstrMonoid RefiningM

runCheckingM :: CheckingM a -> RefiningM (a, Cstr)
runCheckingM m = FlexM.markSection [FlexM.FlexMarkStep "runCheckingM" Nothing] do
  second (\(CstrMonoid cs) -> cs) <$> Writer.runWriterT m

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
  FlexM.mark [FlexM.FlexMarkStep "synthCheckTerm" . Just $ pPrint tm <+> ": ? <:" <+> pPrint tyExpect]
  tm' <- synthTerm tm
  tySynth <- inferTerm tm'
  checkSubtype tm' tySynth tyExpect
  return tm'

-- ** Synthesizing

-- TODO: TermStructure and TermMember don't properly freshen the binds of the
-- child refinements -- gotta do that

synthTerm :: Term Base.Type -> CheckingM (Term TypeReft)
synthTerm term = FlexM.markSectionResult [FlexM.FlexMarkStep "synthTerm" . Just $ pPrint term] True pPrint do
  case term of
    TermNeutral symId args ty -> do
      args' <- synthTerm `traverse` args
      -- TODO: for transforms, input values can't affect output refinement
      -- type, BUT, newtype/variant/enum constructors should have their args
      -- reflected in their type via `C1(a, b, c) : { X : C | X = C1(a, b, c)
      -- }`
      ty' <- transType ty
      return $ TermNeutral symId args' ty'
    TermLiteral lit ty -> do
      -- literals are reflected
      ty' <- transType ty
      tm' <-
        mapM_termAnn (mapM_typeAnn (reflectLiteralInReft (void ty') lit)) $
          TermLiteral lit ty'
      return tm'
    TermPrimitive prim ty ->
      synthPrimitive term ty prim
    TermAssert tm1 tm2 _ty -> do
      -- check asserted term against refinement type { x | x == true }
      tySpec <- reftTypeIsTrue
      tm1' <- synthCheckTerm tySpec tm1
      tm2' <- synthTerm tm2
      ty' <- inferTerm tm2'
      return $ TermAssert tm1' tm2' ty'
    TermLet symId tm bod ty -> do
      tm' <- synthTerm tm

      bod' <- do
        -- p: symId == tm'
        p <- eqPred (varTerm symId (void $ termAnn tm')) (void <$> tm')
        -- the constraint yielded by checking the body must be wrapped in a
        -- quantification over the binding introduced by the let
        bSort <- embedType $ void $ termAnn tm'
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

      ty' <- transType ty

      return $ TermLet symId tm' bod' ty'
    TermStructure {..} -> FlexM.markSection [FlexM.FlexMarkStep "TermStructure" Nothing] do
      -- Convert
      --
      -- @ assert(p(x, y, z)) @
      --
      -- into
      --
      -- @ let x = a; let y = b; let z = c; p(x, y, z) @
      --
      -- and then check that this term satisfies refinement type
      --
      -- @ { VV: bit | VV == true } @

      -- First. get the (translated, but not refined) refinement of the
      -- structure
      tmReft <- getRefinedType' $ Base.structureId termStructure
      -- Nest the assertion body in local bindings which instantiate the values
      -- of the fields
      let tmReft' =
            foldr
              ( \(fieldId, tmField) te -> do
                  let symId = fromSymbolToSymId $ F.symbol fieldId
                  TermLet symId tmField te Base.TypeBit
              )
              tmReft
              termFields

      -- check that the constructed refinement satisfies @{ VV: bit | VV ==
      -- true}@

      do
        tySpec <- reftTypeIsTrue
        void $ synthCheckTerm tySpec tmReft'

      -- check the fields
      termFields' <-
        forM
          (termFields `zip` Base.structureFields termStructure)
          \((fieldId, tmField), (fieldId', tyField)) -> do
            unless (fieldId == fieldId') $ FlexM.throw $ "field ids are not in matching order between the structure constructor and its annotated structure type:" <+> pPrint term
            tyField' <- transType tyField
            (fieldId,) <$> synthCheckTerm tyField' tmField

      ty <- transType termAnn

      let tm =
            TermStructure
              { termStructure,
                termFields = termFields',
                termAnn = ty
              }

      -- reflect in the refinement that this term is equal to it's reflection
      mapM_termAnn (mapM_typeAnn $ reflectTermInReft (void <$> tm)) tm
    TermMember struct@Base.Structure {..} tm termFieldId ty -> FlexM.markSection [FlexM.FlexMarkStep "TermMember" Nothing] do
      ty' <- transType ty
      $(FlexM.debugThing True [|pPrint|] [|ty'|])
      structTerm <- synthCheckTerm (TypeStructure struct mempty) tm
      -- struct
      -- structType: { x: S | p(x) }
      structType <- inferTerm structTerm
      $(FlexM.debugThing True [|pPrint|] [|structType|])
      -- structSort
      structSort <- embedType structType
      $(FlexM.debugThing True [|F.pprint|] [|structSort|])
      -- structReft: { x | p(x) }
      let structReft = typeAnn structType
      $(FlexM.debugThing True [|F.pprint|] [|structReft|])
      -- structSymId: struct
      structSymId <- lift $ freshSymId "structTermMemberInput"
      $(FlexM.debugThing True [|pPrint|] [|structSymId|])

      -- p1(struct): p(struct)
      let p1 =
            F.substa (\sym -> if sym == F.reftBind structReft then symIdSymbol structSymId else sym) $
              F.reftPred structReft
      $(FlexM.debugThing True [|F.pprint|] [|p1|])

      -- structure constructor
      constrSymId <- fromSymbolToSymId . F.val <$> structureSymbol structureId
      $(FlexM.debugThing True [|pPrint|] [|constrSymId|])

      -- other fields
      fieldSymIds <- forM structureFields $ lift . freshSymIdTermId . Base.fromFieldIdToTermId . fst
      fieldTypeRefts <- forM structureFields $ transType . snd
      fieldSorts <- forM fieldTypeRefts embedType
      let fieldTerms = uncurry varTerm <$> fieldSymIds `zip` fieldTypeRefts
      $(FlexM.debugThing True [|pPrint|] [|fieldTerms|])

      -- this
      let thisSym = F.reftBind $ typeAnn ty'
      let thisSymId = fromSymbolToSymId thisSym
      $(FlexM.debugThing True [|pPrint|] [|thisSymId|])

      let args =
            (structureFields `zip` fieldTerms) <&> \((fieldId, _), fieldTerm) -> do
              if fieldId == termFieldId
                then varTerm thisSymId (void ty')
                else void <$> fieldTerm
      $(FlexM.debugThing True [|pPrint|] [|args|])

      -- p2(x1, ..., x[i-1], x[i+1], ..., xn, struct, this): struct == S x1 ... x[i-1] this x[i+1] xn
      p2 <-
        eqPred
          (varTerm structSymId $ void ty')
          (TermNeutral constrSymId args $ void ty')
      $(FlexM.debugThing True [|F.pprint|] [|p2|])

      -- p(this): exists x1 ... xn struct . p1(struct) && p2(x1, ..., xn, struct, this)
      let p =
            F.pExist
              ( (symIdSymbol structSymId, structSort)
                  : ( filter
                        (\(symId, _) -> symId /= thisSymId)
                        (fieldSymIds `zip` fieldSorts)
                        <&> first symIdSymbol
                    )
              )
              $ conjPred [p1, p2]
      $(FlexM.debugThing True [|F.pprint|] [|p|])

      -- FlexM.debugMark True $
      --   FlexM.FlexMarkStep "TermMember" . Just $
      --     vcat
      --       [ "structSymId =" <+> F.pprint (symIdSymbol structSymId),
      --         "p1 =" <+> F.pprint p1,
      --         "p2 =" <+> F.pprint p2,
      --         "p =" <+> F.pprint p,
      --         "F.reft thisSym p =" <+> F.pprint (F.reft thisSym p)
      --       ]

      let ty'' = ty' {typeAnn = F.reft thisSym p}
      $(FlexM.debugThing True [|pPrint|] [|ty''|])

      return $
        TermMember
          { termStructure = struct,
            termTerm = structTerm,
            termFieldId,
            termAnn = ty''
          }

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
      tyTuple <- tupleTypeReft [ty1, ty2]
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
      ty' <- transType ty
      mapM_termAnn
        (mapM_typeAnn $ reflectPrimitiveInReft (void ty') (void <$> prim))
        $ TermPrimitive prim ty'

    go1 constr tm = do
      tm' <- synthTerm tm
      let prim = constr tm'
      ty' <- transType ty
      mapM_termAnn
        (mapM_typeAnn $ reflectPrimitiveInReft (void ty') (void <$> prim))
        $ TermPrimitive prim ty'

    go3 constr tm1 tm2 tm3 = do
      tm1' <- synthTerm tm1
      tm2' <- synthTerm tm2
      tm3' <- synthTerm tm3
      let prim = constr tm1' tm2' tm3'
      ty' <- transType ty
      mapM_termAnn
        (mapM_typeAnn $ reflectPrimitiveInReft (void ty') (void <$> prim))
        $ TermPrimitive prim ty'

-- ** Reflection

-- | Given a term and a refinement type over that term's type, include in the
-- refinement that the value is equal to the (embedded) term.
--
-- > reflectTermInReft v { x: a | r } = { x: a | x == v && r }
reflectTermInReft :: Term (Type ()) -> F.Reft -> CheckingM F.Reft
reflectTermInReft tm r = FlexM.markSectionResult [FlexM.FlexMarkStep "reflectTermInReft" . Just $ pPrint tm] True F.pprint do
  let sort = void $ termAnn tm
  let x = F.reftBind r
  let p = F.reftPred r
  pRefl <-
    embedTerm $
      TermPrimitive
        (PrimitiveEq (varTerm (fromSymbolToSymId x) sort) tm)
        (typeBit ())
  return $ F.reft x (conjPred [pRefl, p])

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
  FlexM.mark [FlexM.FlexMarkStep "checkSubtype" . Just $ pPrint tmSynth $$ nest 2 (" :" <+> pPrint tySynth) $$ nest 2 ("<:" <+> pPrint tyExpect)]

  --    forall x : T, p x ==> (p' x')[x' := x]
  --  ----------------------------------------------
  --    {x : T | p x} <: {x' : T | p' y'}
  tellCstr
    =<< ( cstrForall xSynth tySynth $
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

-- ** Utility Refined Types

-- @{ VV: bit | VV == true }@
reftTypeIsTrue :: CheckingM (Type F.Reft)
reftTypeIsTrue =
  mapM_typeAnn (reflectLiteralInReft (TypeAtomic TypeBit ()) (LiteralBit True))
    =<< transType Base.TypeBit