{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Language.Flex.Refining.Check where

-- TODO: rename this module to "Refining"

import Control.Lens (at, (&), (^.))
import Control.Monad
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans (lift)
import qualified Control.Monad.Writer as Writer
import qualified Control.Monad.Writer.Class as Writer
import Data.Bifunctor
import Data.Functor
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Types as F
import Language.Flex.FlexM (FlexM, freshSymbol, markSection)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Constraint
import Language.Flex.Refining.Logic (conjPred, replaceSym)
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import qualified Language.Flex.Refining.Syntax as Syntax
import Language.Flex.Refining.Translating (embedTerm, embedType, eqPred, structureSymbol, transTerm, transType, tupleTypeReft)
import Language.Flex.Syntax (Literal (..))
import qualified Language.Flex.Syntax as Base
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), comma, hcat, hsep, nest, parens, punctuate, render, space, text, vcat, ($$), (<+>))
import Utility (for, pprintInline, ticks)

type CheckingM = Writer.WriterT CstrMonoid RefiningM

runCheckingM :: CheckingM a -> RefiningM (a, Cstr)
runCheckingM m = FlexM.markSection [FlexM.FlexMarkStep "runCheckingM" Nothing] do
  (a, CstrMonoid cstr es) <- Writer.runWriterT m
  FlexM.debugMark False $ FlexM.FlexMarkStep "constraint expressions" . Just $ nest 2 $ vcat $ F.pprint <$> es
  return (a, cstr)

data CstrMonoid = CstrMonoid Cstr [F.Expr]

mapCstrMonoid :: (Cstr -> Cstr) -> CstrMonoid -> CstrMonoid
mapCstrMonoid f (CstrMonoid cstr tms) = CstrMonoid (f cstr) tms

tellCstr :: (FlexM.MonadFlex' m, Writer.MonadWriter CstrMonoid m) => Cstr -> TypeReft -> m ()
tellCstr cstr ty = do
  FlexM.liftFlex . FlexM.debugMark False . FlexM.FlexMarkStep "tellCstr" . Just $ pPrint ty
  Writer.tell $ CstrMonoid cstr [qreftPred $ typeAnn ty]

tellIntro :: H.Bind RefiningError -> CheckingM a -> CheckingM a
tellIntro bnd m = do
  (a, CstrMonoid _cstr es) <- Writer.listen $ flip Writer.censor m \(CstrMonoid cstr es) ->
    CstrMonoid (H.All bnd cstr) es
  FlexM.liftFlex . FlexM.debugMark False . FlexM.FlexMarkStep "tellIntro" . Just $
    "forall" <+> F.pprint (H.bSym bnd) <+> "." <+> (hcat $ punctuate (comma <> space) $ F.pprint <$> es)
  return a

instance Semigroup CstrMonoid where
  CstrMonoid cstr1 tms1 <> CstrMonoid cstr2 tms2 = CstrMonoid (andCstr cstr1 cstr2) (tms1 <> tms2)

instance Monoid CstrMonoid where
  mempty = CstrMonoid trivialCstr mempty

-- ** Checking

synthCheckTerm :: TypeReft -> Term Base.Type -> CheckingM (Term TypeReft)
synthCheckTerm tyExpect tm = FlexM.markSection [FlexM.FlexMarkStep "synthCheckTerm" . Just $ pPrint tm <+> ": ? <:" <+> pPrint tyExpect] do
  tm' <- synthTerm tm
  tySynth <- inferTerm tm'
  checkSubtype tm' tySynth tyExpect
  return tm'

-- ** Synthesizing

-- TODO: TermStructure and TermMember don't properly freshen the binds of the
-- child refinements -- gotta do that

synthTerm :: Term Base.Type -> CheckingM (Term TypeReft)
synthTerm term = FlexM.markSectionResult True [FlexM.FlexMarkStep "synthTerm" . Just $ pPrint term] pPrint term pPrint do
  case term of
    TermNeutral symId args ty -> do
      -- neutrals are reflected (functions are already inlined)
      args' <- synthTerm `traverse` args
      ty' <- transType ty
      let tm = TermNeutral symId args' ty'
      tm' <- fmap_termAnn (fmap_typeAnn (reflectTermInReft (void <$> tm))) tm
      return tm'
    TermLiteral lit ty -> do
      -- literals are reflected
      ty' <- transType ty
      tm' <-
        fmap_termAnn (fmap_typeAnn (reflectLiteralInReft (void ty') lit)) $
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
    TermLet symId tm bod _ty -> FlexM.markSection [FlexM.FlexMarkStep ("TermLet " <> render (pPrint symId)) Nothing] do
      tm' <- synthTerm tm

      bod' <- do
        -- p: symId == tm'
        p <- eqPred (varTerm symId (void $ termAnn tm')) (void <$> tm')
        -- the constraint yielded by checking the body must be wrapped in a
        -- quantification over the binding introduced by the let
        let bSym = symIdSymbol symId
        bSort <- embedType $ void $ termAnn tm'
        let bPred = H.Reft p
        tellIntro
          H.Bind
            { bSym,
              bSort,
              bPred,
              bMeta = RefiningError $ F.pprint bSym <+> ":" <+> pprintInline bPred
            }
          $ synthTerm bod

      ty' <- inferTerm bod'
      -- TODO: use ty (??)

      return $ TermLet symId tm' bod' ty'
    --
    TermStructure {..} -> FlexM.markSection [FlexM.FlexMarkStep "TermStructure" Nothing] do
      -- check the structure refinement
      do
        -- TODO: rewrite this to convert into @ let x = a; let y = b; let z = c;
        -- assert p(x, y, z); true : { VV: bit | VV == true } @

        -- First. get the (translated, but not refined) refinement of the
        -- structure
        specTerm <- getRefinedType' $ Base.structureId termStructure

        $(FlexM.debugThing False [|pPrint|] [|specTerm|])

        -- Nest the assertion within local bindings which instantiate the values
        -- of the fields
        let tmAsrt =
              foldr
                ( \(fieldId, fieldTerm) te -> do
                    -- don't freshen, since @specTerm@ already refers to the raw
                    -- field name
                    let symId = fromSymbolToSymId $ F.symbol fieldId
                    TermLet symId fieldTerm te Base.TypeBit
                )
                ( TermAssert
                    specTerm
                    (TermLiteral (LiteralBit True) Base.TypeBit)
                    Base.TypeBit
                )
                termFields

        $(FlexM.debugThing False [|pPrint|] [|tmAsrt|])

        -- check @tmAsrt@; the final value is unimportant, but want to invoke a
        -- check on the assertion within the scope of the @TermLet@s

        void $ synthCheckTerm (typeBit mempty) tmAsrt

      -- check the fields
      fields <-
        forM
          (termFields `zip` Base.structureFields termStructure)
          \((fieldId, fieldTerm), (fieldId', fieldType)) -> do
            unless (fieldId == fieldId') $ FlexM.throw $ "field ids are not in matching order between the structure constructor and its annotated structure type:" <+> pPrint term
            fieldType' <- transType fieldType
            -- > fieldTerm' := tm : { x : a | p(x) }
            fieldTerm' <- synthCheckTerm fieldType' fieldTerm
            -- substitute the bind in @fieldTerm@'s refinement for @F.symbol
            -- fieldId@
            -- > fieldTerm' := tm : { fieldId : a | p(fieldId) }
            fieldTerm'' <-
              lift $
                fmap_termAnn
                  (fmap_typeAnn (return . setQReftBind (F.symbol fieldId)))
                  fieldTerm'
            $(FlexM.debugThing False [|pPrint|] [|(fieldId, fieldTerm'')|])
            return (fieldId, fieldTerm'')

      fieldSorts <- lift $ forM fields (embedType . Syntax.termAnn . snd)

      -- existentially quantify the fields
      let qr =
            QReft
              { qreftQuants =
                  (fields `zip` fieldSorts) <&> \((_fieldId, fieldTerm), fieldSort) ->
                    let qr' = typeAnn $ Syntax.termAnn fieldTerm
                        bSym = qreftBind qr'
                        bPred = H.Reft $ qreftPred qr'
                     in QuantExists
                          H.Bind
                            { bSym,
                              bSort = fieldSort,
                              bPred,
                              bMeta = RefiningError $ "forall" <+> F.pprint bSym <+> ":" <+> F.pprint bPred
                            },
                -- -- substitutes the old bind for `F.symbol fieldId`
                -- let x = F.symbol fieldId
                --     r = qreftReft $ typeAnn $ Syntax.termAnn fieldTerm
                --     p = F.substa (replaceSym (F.reftBind r) x) $ F.reftPred r
                --  in QuantExists
                --       H.Bind
                --         { bSym = x,
                --           bSort = fieldSort,
                --           bPred = H.Reft p,
                --           bMeta = RefiningError $ F.pprint x <+> ":" <+> pprintInline p
                --         },
                qreftReft = mempty
              }

      $(FlexM.debugThing False [|pPrint|] [|qr|])

      let tm =
            TermStructure
              { termStructure,
                termFields = fields,
                termAnn = TypeStructure termStructure qr
              }

      -- reflect in the refinement that this term is equal to it's reflection
      fmap_termAnn (fmap_typeAnn $ reflectTermInReft (void <$> tm)) tm
    --
    TermMember struct@Base.Structure {..} tm termFieldId ty -> FlexM.markSection [FlexM.FlexMarkStep "TermMember" Nothing] do
      ty' <- transType ty
      $(FlexM.debugThing False [|pPrint|] [|ty'|])
      structTerm <- synthCheckTerm (TypeStructure struct mempty) tm
      -- struct
      -- structType: { x: S | p(x) }
      structType <- inferTerm structTerm
      $(FlexM.debugThing False [|pPrint|] [|structType|])
      -- structSort
      structSort <- embedType structType
      $(FlexM.debugThing False [|F.pprint|] [|structSort|])
      -- structQReft: { x | p(x) }
      structQReft <- lift $ freshenQReftBind (typeAnn structType)
      $(FlexM.debugThing False [|pPrint|] [|structQReft|])
      -- structSymId: struct
      structSymId <- lift $ freshSymId "structTermMemberInput"
      $(FlexM.debugThing False [|pPrint|] [|structSymId|])

      -- p1(struct): p(struct)
      let p1 =
            F.substa
              (replaceSym (qreftBind structQReft) (symIdSymbol structSymId))
              (qreftPred structQReft)
      $(FlexM.debugThing False [|F.pprint|] [|p1|])

      -- structure constructor
      constrSymId <- fromSymbolToSymId . F.val <$> structureSymbol structureId
      $(FlexM.debugThing False [|pPrint|] [|constrSymId|])

      -- other fields
      fieldSymIds <- forM structureFields $ lift . freshSymIdTermId . Base.fromFieldIdToTermId . fst
      fieldTypeRefts <- forM structureFields $ transType . snd
      fieldSorts <- forM fieldTypeRefts embedType
      let fieldTerms = uncurry varTerm <$> fieldSymIds `zip` fieldTypeRefts
      $(FlexM.debugThing False [|pPrint|] [|fieldTerms|])

      -- this
      let thisSym = qreftBind $ typeAnn ty'
      let thisSymId = fromSymbolToSymId thisSym
      $(FlexM.debugThing False [|pPrint|] [|thisSymId|])

      let args =
            (structureFields `zip` fieldTerms) <&> \((fieldId, _), fieldTerm) -> do
              if fieldId == termFieldId
                then varTerm thisSymId (void ty')
                else void <$> fieldTerm
      $(FlexM.debugThing False [|pPrint|] [|args|])

      -- p2(x1, ..., x[i-1], x[i+1], ..., xn, struct, this): struct == S x1 ... x[i-1] this x[i+1] xn
      p2 <- eqPred (varTerm structSymId $ void ty') (TermNeutral constrSymId args $ void ty')
      $(FlexM.debugThing False [|F.pprint|] [|p2|])

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
      $(FlexM.debugThing False [|F.pprint|] [|p|])

      -- FlexM.debugMark False $
      --   FlexM.FlexMarkStep "TermMember" . Just $
      --     vcat
      --       [ "structSymId =" <+> F.pprint (symIdSymbol structSymId),
      --         "p1 =" <+> F.pprint p1,
      --         "p2 =" <+> F.pprint p2,
      --         "p =" <+> F.pprint p,
      --         "F.reft thisSym p =" <+> F.pprint (F.reft thisSym p)
      --       ]

      let ty'' = ty' {typeAnn = fromReft $ F.reft thisSym p}
      $(FlexM.debugThing False [|pPrint|] [|ty''|])

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
      fmap_termAnn
        (fmap_typeAnn $ reflectPrimitiveInReft (void ty') (void <$> prim))
        $ TermPrimitive prim ty'

    go1 constr tm = do
      tm' <- synthTerm tm
      let prim = constr tm'
      ty' <- transType ty
      fmap_termAnn
        (fmap_typeAnn $ reflectPrimitiveInReft (void ty') (void <$> prim))
        $ TermPrimitive prim ty'

    go3 constr tm1 tm2 tm3 = do
      tm1' <- synthTerm tm1
      tm2' <- synthTerm tm2
      tm3' <- synthTerm tm3
      let prim = constr tm1' tm2' tm3'
      ty' <- transType ty
      fmap_termAnn
        (fmap_typeAnn $ reflectPrimitiveInReft (void ty') (void <$> prim))
        $ TermPrimitive prim ty'

-- ** Reflection

-- | Given a term and a refinement type over that term's type, include in the
-- refinement that the value is equal to the (embedded) term.
--
-- > reflectTermInReft v { x: a | r } = { x: a | x == v && r }
reflectTermInReft :: Term (Type ()) -> QReft -> CheckingM QReft
reflectTermInReft tm qr = FlexM.markSectionResult True [FlexM.FlexMarkStep "reflectTermInReft" . Just $ pPrint tm] pPrint tm pPrint do
  let r = qreftReft qr
  let sort = void $ termAnn tm
  let x = F.reftBind r
  let p = F.reftPred r
  pRefl <-
    embedTerm $
      TermPrimitive
        (PrimitiveEq (varTerm (fromSymbolToSymId x) sort) tm)
        (typeBit ())
  return $ qr {qreftReft = F.reft x (conjPred [pRefl, p])}

reflectLiteralInReft ty lit = reflectTermInReft (TermLiteral lit ty)

reflectPrimitiveInReft ty prim = reflectTermInReft (TermPrimitive prim ty)

-- ** Inferring

inferTerm :: Term TypeReft -> CheckingM TypeReft
inferTerm term = FlexM.markSectionResult True [FlexM.FlexMarkStep "inferTerm" . Just $ pPrint term] pPrint term pPrint do
  return $ termAnn term

-- ** Subtyping

checkSubtype :: Term TypeReft -> TypeReft -> TypeReft -> CheckingM ()
checkSubtype tmSynth tySynth tyExpect = FlexM.markSection [FlexM.FlexMarkStep "checkSubtype" . Just $ pPrint tmSynth $$ nest 2 (" :" <+> pPrint tySynth) $$ nest 2 ("<:" <+> pPrint tyExpect)] do
  --    forall y : a, (p y) ==> (p' x')[x' := y]
  --  ----------------------------------------------
  --    { x : a | p x } <: { x' : a | p' x' }

  -- use fresh symbol for the constrained variable that is propogated upwards
  y <- freshSymbol xSynth
  let rho = xExpect `replaceSym` y
  let pExpect' = F.substa rho pExpect
  let tyExpect' = F.substa rho tyExpect
  -- let pSynth' = subst pSynth xSynth y
  let tySynth' = F.substa rho tySynth
  let tmSynth' = tmSynth {termAnn = tySynth'}
  let pSpec = pExpect'

  FlexM.debugMark True $
    FlexM.FlexMarkStep "checkSubtype locals" . Just $
      vcat
        [ "y         =" <+> F.pprint y,
          "tySynth'  =" <+> pPrint tySynth',
          "tmSynth'  =" <+> pPrint tmSynth',
          "pExpect'  =" <+> F.pprint pExpect',
          "tyExpect' =" <+> pPrint tyExpect'
        ]

  -- TODO: more properly extract quantifiers here??
  -- TODO: probably want to extract Lets as equations or something but for now just use refinements with euqalities in them
  cstr <-
    cstrForall y tySynth' $
      cstrHead tmSynth' tyExpect' pSpec
  tellCstr cstr tySynth'
  where
    rSynth = qreftReft $ typeAnn tySynth
    rExpect = qreftReft $ typeAnn tyExpect
    (xSynth, _pSynth) = (F.reftBind rSynth, F.reftPred rSynth)
    (xExpect, pExpect) = (F.reftBind rExpect, F.reftPred rExpect)

-- ** Utility Refined Types

-- @{ VV: bit | VV == true }@
reftTypeIsTrue :: CheckingM TypeReft
reftTypeIsTrue =
  fmap_typeAnn (reflectLiteralInReft (TypeAtomic TypeBit ()) (LiteralBit True))
    =<< transType Base.TypeBit
