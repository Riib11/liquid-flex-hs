{-# LANGUAGE TemplateHaskell #-}

{-# HLINT ignore "Redundant flip" #-}
{-# HLINT ignore "Redundant $" #-}

module Language.Flex.Refining.Translating where

import Control.Lens (At (at), locally, to, (&), (?~), (^.), _3)
import Control.Monad (filterM, foldM, forM, void, when)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), runReader)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (foldlM)
import Data.Functor
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (pack)
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Types as F
import Language.Fixpoint.Types.PrettyPrint as P
import Language.Flex.FlexM (FlexM, MonadFlex, defaultLocated, freshSymbol, freshenSymbol)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Logic (conjPred, replaceSym)
import Language.Flex.Refining.Prelude (tupleFTycon, tupleTermConstructorSymbol)
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import Language.Flex.Syntax (Literal (..), renameTerm)
import qualified Language.Flex.Syntax as Base
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility

-- ** Translate Term

transTerm :: Base.Term Base.Type -> RefiningM (Term Base.Type)
transTerm term = FlexM.markSection [FlexM.FlexMarkStep "transTerm" . Just $ pPrint term] do
  case term of
    Base.TermLiteral lit ty -> return $ TermLiteral lit ty
    Base.TermPrimitive prim ty ->
      case prim of
        Base.PrimitiveTry te -> TermPrimitive <$> (PrimitiveTry <$> transTerm te) <*> return ty
        Base.PrimitiveTuple tes | length tes < 2 -> FlexM.throw "attempted to transTerm on a Base.PrimitiveTuple that has length terms < 2"
        Base.PrimitiveTuple (te : tes) -> do
          te' <- transTerm te
          let f :: Term Base.Type -> Base.Term Base.Type -> RefiningM (Term Base.Type)
              f tm1' tm2 = do
                tm2' <- transTerm tm2
                return $
                  TermPrimitive
                    (PrimitiveTuple (tm1', tm2'))
                    (Base.TypeTuple [termAnn tm1', termAnn tm2'])
          foldlM f te' tes -- TUPLE: fold left
        Base.PrimitiveTuple _ -> error "IMPOSSIBLE"
        Base.PrimitiveArray tes -> TermPrimitive <$> (PrimitiveArray <$> transTerm `traverse` tes) <*> return ty
        Base.PrimitiveIf te te' te3 -> TermPrimitive <$> (PrimitiveIf <$> transTerm te <*> transTerm te' <*> transTerm te3) <*> return ty
        Base.PrimitiveAnd te te' -> TermPrimitive <$> (PrimitiveAnd <$> transTerm te <*> transTerm te') <*> return ty
        Base.PrimitiveOr te te' -> TermPrimitive <$> (PrimitiveOr <$> transTerm te <*> transTerm te') <*> return ty
        Base.PrimitiveNot te -> TermPrimitive <$> (PrimitiveNot <$> transTerm te) <*> return ty
        Base.PrimitiveEq te te' -> TermPrimitive <$> (PrimitiveEq <$> transTerm te <*> transTerm te') <*> return ty
        Base.PrimitiveAdd te te' -> TermPrimitive <$> (PrimitiveAdd <$> transTerm te <*> transTerm te') <*> return ty
        -- invalid
        Base.PrimitiveCast _ -> FlexM.throw $ "PrimitiveCast should not appear in typed term:" <+> pPrint term
    -- local binding is added to refinement context during refinement checking,
    -- not translation (since the implementation of the let needs to be checked
    -- first)
    Base.TermLet {termPattern, termTerm, termBody} -> do
      symId <- case termPattern of
        Base.PatternNamed ti _ty -> freshSymIdTermId ti
        Base.PatternDiscard _ty -> freshSymId "discard"
      tm <- transTerm termTerm
      ty <- transType $ termAnn tm
      bod <-
        comps
          [ introSymId symId,
            introApplicantType symId (Base.ApplicantType ty)
          ]
          $ transTerm termBody
      return $ TermLet symId tm bod (termAnn bod)
    Base.TermAssert {termTerm, termBody} -> do
      tm <- transTerm termTerm
      bod <- transTerm termBody
      return $ TermAssert tm bod (termAnn bod)
    Base.TermStructure {..} -> do
      fields' <- forM termFields . secondM $ transTerm
      termStructure <- getStructure termStructureId
      return
        TermStructure
          { termStructure,
            termFields = fields',
            termAnn
          }
    Base.TermMember te fi ty -> do
      -- this has to be a special form rather than NeutralTerm because
      -- NeutralTerm doesn't deal with functions that have dependent refinement
      -- types
      te' <- transTerm te
      struct <- case termAnn te' of
        Base.TypeStructure struct -> return struct
        _ -> FlexM.throw $ "a TermMember's term should have a Structure type, but instead has type:" <+> pPrint ty
      return $ TermMember struct te' fi ty
    Base.TermNeutral app mb_args mb_cxargs ty -> transNeutral app mb_args mb_cxargs ty
    Base.TermMatch _te _x0 _ty -> error "transTerm TermMatch"
    -- invalid
    Base.TermAscribe _te _ty _ty' -> FlexM.throw $ "term ascribe should not appear in typed term:" <+> pPrint term

transNeutral :: Base.Applicant a -> Maybe [Base.Term Base.Type] -> Maybe [Base.Term Base.Type] -> Base.Type -> RefiningM (Term Base.Type)
transNeutral app mb_args mb_cxargs ty = do
  symId <- getSymId (void app)
  -- note that we avoid inserting the globally-known refinement types here,
  -- since we will do that in the refining step anyway, and we must provide a
  -- `Base.Type` right now anyway since we're producing a `Term Base.Type`
  getApplicantType symId >>= \case
    -- Function application is inlined. For example, given
    --
    -- @ function f(x: bit) -> bit { let y = !x; y } @
    --
    -- then the application @f(true)@ is inlined to be
    --
    -- @ let x' = true; let y' = !x'; y' @
    --
    -- where @x'@ and @y'@ are fresh variables substituted in for @x@ and @y@
    Base.ApplicantTypeFunction Base.FunctionType {..} | not functionTypeIsTransform -> do
      -- mb_args' <- (transTerm `traverse`) `traverse` mb_args
      -- mb_cxargs' <- (transTerm `traverse`) `traverse` mb_cxargs
      getFunction symId >>= \Base.Function {..} -> do
        -- make fresh versions of arg ids
        -- RefiningM (Map.Map Base.TermId Base.TermId)
        fresheningArgs <-
          Map.fromList
            <$> case mb_args of
              Nothing -> return []
              Just args ->
                forM (functionParameters `zip` args) \((argId, _ty), arg) -> do
                  argSymId <- freshenTermId argId
                  return (argId, (argSymId, arg))

        -- make fresh version of cxarg ids
        fresheningCxargs <-
          Map.fromList <$> case (functionContextualParameters, mb_cxargs) of
            (Nothing, Nothing) -> return []
            (Just cxparams, Just cxargs) -> forM (cxparams `zip` cxargs) \((_tyId, argId), cxarg) -> do
              argSymId <- freshenTermId argId
              return (argId, (argSymId, cxarg))
            _ -> FlexM.throw $ "function type's contextual parameters doesn't correspond to application's contextual arguments: " <+> pPrint functionContextualParameters <+> "," <+> pPrint mb_cxargs

        -- argId => (argSymId, tm)
        let freshening = Map.union fresheningArgs fresheningCxargs

        -- argId => argSymId
        let renaming = fst <$> freshening

        do
          let renaming' = Map.toList renaming
          $(FlexM.debugThing True [|pPrint|] [|renaming'|])

        $(FlexM.debugThing True [|pPrint|] [|functionBody|])

        -- rename via `renaming` in `functionBody`
        let functionBody' :: Base.Term Base.Type
            functionBody' =
              comps
                ( Map.elems freshening <&> \(argSymId, arg') tm ->
                    Base.TermLet (Base.PatternNamed argSymId (Base.termAnn arg')) arg' tm (Base.termAnn tm)
                )
                $ renameTerm renaming functionBody

        $(FlexM.debugThing True [|pPrint|] [|functionBody'|])

        transTerm functionBody'
    -- non-functions are not inlined
    _ -> do
      mb_args' <- (transTerm `traverse`) `traverse` mb_args
      mb_cxargs' <- (transTerm `traverse`) `traverse` mb_cxargs
      let args'' = fromMaybe [] mb_args' ++ fromMaybe [] mb_cxargs'
      return $ TermNeutral symId args'' ty

-- !TODO put the stuff for transTypeRefinement (from
-- Language.Flex.Refining.Refining) here

transRefinedTypeRefinement ::
  -- | local variables
  [(Base.TermId, Base.Type)] ->
  Base.Refinement Base.Type ->
  RefiningM (Term Base.Type)
transRefinedTypeRefinement locals reft = do
  comps
    ( -- introduce local variables into context
      locals <&> \(tmId, ty) m -> do
        -- not fresh, because the refinement can refer to it
        let symId =
              SymId
                { symIdSymbol = F.symbol tmId,
                  symIdMaybeTypeId = Nothing,
                  symIdMaybeTermId = Just tmId
                }
        ty' <- transType ty
        comps
          [ introSymId symId,
            introApplicantType symId (Base.ApplicantType ty')
          ]
          m
    )
    $ transTerm (reft & Base.unRefinement)

transType :: MonadFlex m => Base.Type -> m TypeReft
transType type_ = FlexM.markSection [FlexM.FlexMarkStep "transType" . Just $ pPrint type_] case type_ of
  Base.TypeNumber numty n -> do
    x <- freshSymbol (render $ pPrint type_)
    let p = case numty of
          Base.TypeInt ->
            -- -2^(n-1) < x < 2^(n-1)
            conjPred
              [ F.PAtom F.Le (F.expr (-(2 ^ (n - 1)) :: Int)) (F.expr x),
                F.PAtom F.Lt (F.expr x) (F.expr (2 ^ (n - 1) :: Int))
              ]
          Base.TypeUInt ->
            -- 0 <= x < 2^n
            conjPred
              [ F.PAtom F.Le (F.expr (0 :: Int)) (F.expr x),
                F.PAtom F.Lt (F.expr x) (F.expr (2 ^ n :: Int))
              ]
          Base.TypeFloat -> error "!TODO transType TypeFloat"
    let atomic = case numty of
          Base.TypeInt -> TypeInt
          Base.TypeUInt -> TypeInt
          Base.TypeFloat -> TypeFloat
    return $ TypeAtomic atomic (fromReft $ F.reft x p)
  Base.TypeBit -> return $ TypeAtomic TypeBit mempty
  Base.TypeChar -> return $ TypeAtomic TypeChar mempty
  Base.TypeArray Base.TypeChar -> return $ TypeAtomic TypeString mempty
  Base.TypeArray _ty -> error "transType TypeArray"
  Base.TypeTuple tys -> do
    tupleTypeReft =<< transType `traverse` tys
  Base.TypeOptional _ty -> error "transType TypeOptional"
  Base.TypeNamed ti -> error $ "transType Named: " <> render (pPrint ti)
  Base.TypeStructure struct@Base.Structure {..} -> do
    structureTypeReft struct =<< secondM transType `traverse` structureFields
  Base.TypeEnum _en -> error "transType Enum"
  Base.TypeVariant varnt@Base.Variant {..} ->
    variantTypeReft varnt =<< secondM (transType `traverse`) `traverse` variantConstructors
  Base.TypeNewtype _new -> error "transType Newtype"
  -- invalid
  Base.TypeUnifyVar _ _ -> FlexM.throw $ "type unification variable should not appear in normalized type:" <+> pPrint type_

-- ** Basic Refinement Types

-- | Refined variant type.
--
-- > variantTypeReft ... = ... TODO
variantTypeReft :: MonadFlex m => Base.Variant Base.Type -> [(Base.TermId, [TypeReft])] -> m TypeReft
variantTypeReft varnt@Base.Variant {..} ctors = FlexM.markSectionResult (FlexM.FlexMarkStep "variantTypeReft" . Just $ pPrint variantId) pPrint varnt pPrint do
  varntSym <- freshSymbol ("variantTerm" :: String)

  ctors' <-
    forM ctors \(ctorId, paramTypes) -> do
      paramTypes' <- forM paramTypes \paramType -> do
        flip fmapTop_typeAnn paramType \qr -> do
          paramSym <- freshSymbolFromType paramType
          return $ setQReftBind paramSym qr
      return (ctorId, paramTypes')

  -- > p = exists a1, ..., an, ..., z1, ..., zn . { varnt: V | varnt == ctor_a a1 ...
  -- an || ... || varnt == ctor_z z1 ... zn }

  let varntType = TypeVariant varnt ()

  -- for each constructor @ctor : A1 -> ... -> An -> V@
  ps <- forM ctors' \(ctorId, paramTypes) ->
    -- varnt == ctor x1 ... xn
    eqPred
      (varTerm (fromSymbolToSymId varntSym) varntType)
      --
      ( TermNeutral
          (SymId (F.symbol (variantId, ctorId)) (Just variantId) (Just ctorId))
          (paramTypes <&> \paramType -> varTerm (fromSymbolToSymId (qreftBind $ typeAnn paramType)) (void paramType))
          varntType
      )

  -- !NOTE this will definitely be a source of slowness
  let p = F.pOr ps

  quants <- ((concat . concat) <$>) . forM ctors' $ \(_ctorId, paramTypes) -> do
    forM paramTypes extractQuants

  let qr =
        QReft
          { qreftQuants = quants,
            qreftReft = F.reft varntSym p
          }

  return $ TypeVariant varnt qr

-- | Refined structure type.
--
-- > structureTypeReft ... = ... TODO
structureTypeReft :: MonadFlex m => Base.Structure Base.Type -> [(Base.FieldId, TypeReft)] -> m TypeReft
structureTypeReft struct@Base.Structure {..} fieldTys_ = FlexM.markSectionResult (FlexM.FlexMarkStep "structureTypeReft" . Just $ pPrint structureId <+> "; " <+> pPrint fieldTys_) pPrint struct pPrint do
  structSym <- freshSymbol ("structTermStructure" :: String)

  fieldTys <-
    forM fieldTys_ \(fieldId, ty) -> do
      -- > ty = { x : a | p(x) }
      let x = qreftBind $ typeAnn ty

      -- rename refinement bind to use name defined by field (to be compatible
      -- with checking refinement on structure, so shouldn't freshen)
      let y = F.symbol fieldId

      -- > ty = { y : a | p(y) }
      let ty' = ty {typeAnn = F.substa (replaceSym x y) (typeAnn ty)}

      return (fieldId, ty')

  -- tyStruct: S a1 ... aN
  let tyStruct = TypeStructure struct ()

  -- > p1(struct, x1, ..., xN) = struct = S a1 ... aN
  p1 <-
    eqPred
      (varTerm (fromSymbolToSymId structSym) tyStruct)
      ( TermStructure
          struct
          ( fieldTys <&> \(fieldId, ty) ->
              ( fieldId,
                fromSymbolToTerm (F.reftBind $ qreftReft $ typeAnn ty) (void ty)
              )
          )
          tyStruct
      )

  $(FlexM.debugThing False [|F.pprint|] [|p1|])

  $(FlexM.debugThing False [|pPrint|] [|fieldTys|])

  -- !TODO if the changes look ok, then p1 ~~> p

  -- !TODO OLD, now can just put the predicate on the existential
  -- quantifier for the field

  -- -- p2(x1, ..., xN): r1(x1) && ... && rN(xN)
  -- let p2 = conjPred $ fieldTys <&> \(_, ty) -> F.reftPred $ typeAnn ty

  -- \$(FlexM.debugThing False [|F.pprint|] [|p2|])

  -- -- !TODOOLD: now put existential quantifiers on the refinement

  -- p(struct): exists x1, ..., xN . p1(struct, x1, ..., xN) && p2(x1, ..., xN)
  -- fieldSrts <- secondM embedType `traverse` fieldTys
  -- let p =
  --       F.pExist
  --         ( fieldTys `zip` fieldSrts <&> \((_, ty), (_, srt)) ->
  --             (F.reftBind $ typeAnn ty, srt)
  --         )
  --         $ conjPred [p1, p2]

  -- !TODOOLD: just use p1 now

  -- -- p(struct, x1, ..., xN): p1(struct, x1, ..., xN) && p2(x1, ..., xN)
  -- let p = conjPred [p1, p2]

  -- reftQuants: exists x1, ..., exists nN
  quants <- concat <$> forM fieldTys (extractQuants . snd)

  -- qr: { struct: S a1 ... aN | exists x1, ..., exists nN, p2(struct, x1, ...,
  -- xN) }
  let qr =
        QReft
          { qreftQuants = quants,
            qreftReft = F.reft structSym p1
          }

  $(FlexM.debugThing False [|pPrint|] [|qr|])

  return $ TypeStructure struct qr

-- | Refined tuple type.
--
-- > tupleTypeReft [.., { xI: aI | pI(xI) }, ...] = { tuple: (((a1, a2), ...),
-- > aN) |  (tuple == (((x1, x2), ...), xN)  ) && ... && pI(xI) && .... }
tupleTypeReft :: forall m. MonadFlex m => [TypeReft] -> m TypeReft
tupleTypeReft tys_ = do
  let go :: TypeReft -> TypeReft -> m TypeReft
      go ty1 ty2 = do
        -- ty1: { x1: a1 | r1(x1) }
        -- ty2: { x2: a2 | r2(x2) }

        -- r1(x1)
        -- r2(x2)
        r1 <- freshenReftBind (qreftReft $ typeAnn ty1)
        r2 <- freshenReftBind (qreftReft $ typeAnn ty2)

        symTuple <- freshSymbol ("tuple" :: String)

        -- tyTuple: (ty1, ty2)
        -- unrefined, since only used for embedding
        let tyTuple = TypeTuple (void ty1, void ty2) ()

        -- !TODO if using contraint quantifiers works, then p1 ~~> p

        -- p1(tuple, x1, x2): tuple == (x1, x2)
        p1 <-
          eqPred
            (varTerm (fromSymbolToSymId symTuple) tyTuple)
            ( TermPrimitive
                ( PrimitiveTuple
                    ( fromSymbolToTerm (F.reftBind r1) (void ty1),
                      fromSymbolToTerm (F.reftBind r2) (void ty2)
                    )
                )
                tyTuple
            )

        -- !TODOOLD: don't need to do this anymore because Reft keeps track of
        -- refinement on quantifier vars

        -- -- p2(tuple, x1, x2): r1(x1) && r2(x2)
        -- let p2 = conjPred $ [ty1, ty2] <&> (F.reftPred . typeAnn)

        -- -- r: { tuple: tyTuple | exists x1 x2 . p1(y1, x2) && p2(y1, x2) }
        -- srt1 <- embedType ty1
        -- srt2 <- embedType ty2
        -- let r =
        --       F.reft symTuple $
        --         F.pExist [(F.reftBind r1, srt1), (F.reftBind r2, srt2)] $
        --           conjPred [p1, p2]

        quants <- concat <$> forM [ty1, ty2] extractQuants

        -- r: { tuple: (a, b) | exists x1, exists x2, p1(tuple, x1, x2) }
        let r =
              QReft
                { qreftQuants = quants,
                  qreftReft = F.reft symTuple p1
                }

        -- { tuple: (a1, a2) | p1 && p2 }
        return $ TypeTuple (ty1, ty2) r

  case tys_ of
    [] -> error "tupleTypeReft []"
    [_] -> error "tupleTypeReft [ _ ]"
    (ty : tys) -> foldlM go ty tys -- TUPLE: fold left

-- ** Utilities

-- | The predicate that asserts that two (embedded) terms are equal.
--
-- > eqPred tm1 tm2 = { tm1 == tm2 }
eqPred :: MonadFlex m => Term (Type ()) -> Term (Type ()) -> m F.Pred
eqPred tm1 tm2 =
  embedTerm $
    TermPrimitive
      (PrimitiveEq tm1 tm2)
      (typeBit ())

-- *** Translating to Sorts

-- -- !TODODEPRECATED: is this still needed?
-- sortOfBaseType :: Base.Type -> F.Sort
-- sortOfBaseType = \case
--   Base.TypeNumber nt _n -> case nt of
--     Base.TypeInt -> F.intSort
--     Base.TypeUInt -> F.intSort
--     Base.TypeFloat -> F.realSort
--   Base.TypeBit -> F.boolSort
--   Base.TypeChar -> F.charSort
--   -- !TODO use FApp (type constructor application) and FObj (uninterpreted
--   -- type), and don't need to worry about needing to directly convert TypeIds to
--   -- Symbols since there's never any possible shadoing of TypeIds
--   Base.TypeArray _ty -> error "sortOfBaseType"
--   Base.TypeTuple _tys -> error "sortOfBaseType"
--   Base.TypeOptional _ty -> error "sortOfBaseType"
--   Base.TypeNamed _ti -> error "sortOfBaseType"
--   Base.TypeUnifyVar _uv _m_uc -> error "sortOfBaseType"
--   Base.TypeStructure _struc -> error "sortOfBaseType"
--   Base.TypeEnum _en -> error "sortOfBaseType"
--   Base.TypeVariant _vari -> error "sortOfBaseType"
--   Base.TypeNewtype _new -> error "sortOfBaseType"

-- * Embedding

-- Embedding doesn't require RefiningM.

embedSymId :: MonadFlex m => SymId -> m F.Expr
embedSymId SymId {..} = return $ F.eVar symIdSymbol

-- !TODO need to use a Writer to emit equalities that will be bound by the parent predicate
embedTerm :: MonadFlex m => Term (Type ()) -> m F.Expr
embedTerm = \case
  TermLiteral lit _ -> embedLiteral lit
  TermPrimitive prim _ -> embedPrimitive prim
  TermNeutral x args _ -> do
    x' <- embedSymId x
    args' <- embedTerm `traverse` args
    return
      if null args'
        then x'
        else F.eApps x' args'
  TermAssert _ tm _ -> embedTerm tm
  -- (let x = a in b) ~~> ((fun x => b) a)
  TermLet x tm bod _ -> do
    tm' <- embedTerm tm
    sort <- embedType (termAnn tm)
    bod' <- embedTerm bod
    return $ F.eApps (F.ELam (symIdSymbol x, sort) bod') [tm']
  -- (S { x = a; y = b; }) ~~> (S a b)
  TermStructure {..} -> do
    structExpr <- F.eVar <$> structureSymbol (Base.structureId termStructure)
    termFields' <- embedTerm `traverse` (snd <$> termFields)
    return $ F.eApps structExpr termFields'
  TermMember {..} -> do
    projExpr <- F.eVar <$> structureFieldSymbol (Base.structureId termStructure) termFieldId
    argExpr <- embedTerm termTerm
    return $ F.eApps projExpr [argExpr]

-- !TODODEPRECATED: since transTerm just turns into app of field projector
-- -- x.s ~~> (proj$S#x s)
-- TermMember {..} -> do
--   projExpr <- F.eVar <$> structureFieldProjectorSymbol (Base.structureId termStructure) termFieldId
--   tm' <- embedTerm termTerm
--   return $ F.eApps projExpr [tm']

embedLiteral :: MonadFlex m => Literal -> m F.Expr
embedLiteral =
  return . \case
    Base.LiteralInteger n -> F.expr n
    Base.LiteralFloat _x -> error "!TODO embed float literal"
    Base.LiteralBit b -> if b then F.PTrue else F.PFalse
    Base.LiteralChar _c -> error "!TODO how to embed a literal char? can't just be a Text because of Sort error..." -- F.expr c -- (pack [c])
    Base.LiteralString s -> F.expr (pack s)

embedPrimitive :: MonadFlex m => Primitive (Type ()) -> m F.Expr
embedPrimitive = \case
  PrimitiveTry _ -> error "embedPrimitive Try"
  PrimitiveTuple (tm1, tm2) -> do
    e1 <- embedTerm tm1
    e2 <- embedTerm tm2
    return $ tupleConstructorExpr `F.EApp` e1 `F.EApp` e2
  PrimitiveArray _ -> error "embedPrimitive Array"
  PrimitiveIf te te' te2 -> F.EIte <$> embedTerm te <*> embedTerm te' <*> embedTerm te2
  PrimitiveAnd te te' -> F.PAnd <$> embedTerm `traverse` [te, te']
  PrimitiveOr te te' -> F.POr <$> embedTerm `traverse` [te, te']
  PrimitiveNot te -> F.PNot <$> embedTerm te
  PrimitiveEq te te' -> F.PAtom F.Eq <$> embedTerm te <*> embedTerm te'
  PrimitiveAdd te te' -> F.EBin F.Plus <$> embedTerm te <*> embedTerm te'

embedTermId :: MonadFlex m => Base.TermId -> m F.Symbol
embedTermId tmId = return $ fromString (render . pPrint $ tmId)

-- *** Primitive Constructors

tupleConstructorExpr :: F.Expr
tupleConstructorExpr = F.eVar tupleTermConstructorSymbol

-- ** Embedding as Sorts

embedType :: MonadFlex m => Type r -> m F.Sort
embedType = \case
  TypeAtomic atomic _ -> case atomic of
    TypeInt -> return F.intSort
    TypeFloat -> return F.realSort
    TypeBit -> return F.boolSort
    TypeChar -> return F.charSort
    TypeString -> return F.strSort
  TypeTuple (ty1, ty2) _ -> F.fApp (F.fTyconSort tupleFTycon) <$> (embedType `traverse` [ty1, ty2])
  TypeStructure Base.Structure {..} _ -> F.fTyconSort . F.symbolFTycon <$> structureSymbol structureId
  TypeVariant Base.Variant {..} _ -> F.fTyconSort . F.symbolFTycon <$> variantSymbol variantId

-- ** Datatype

-- *** Structure

-- !TODO change this to be polymorphic so refined argumen types are handled automatically
structureDataDecl :: MonadFlex m => Base.Structure Base.Type -> m F.DataDecl
structureDataDecl Base.Structure {..} =
  do
    dcName <- structureSymbol structureId
    let ddTyCon = F.symbolFTycon dcName
    dcFields <- forM structureFields \(fieldId, ty) -> do
      dfName <- structureFieldSymbol structureId fieldId
      dfSort <- embedType =<< transType ty
      return F.DField {dfName, dfSort}
    return
      F.DDecl
        { ddTyCon,
          ddVars = 0,
          ddCtors = [F.DCtor {dcName, dcFields}]
        }

structureSymbol :: MonadFlex m => Base.TypeId -> m F.LocSymbol
structureSymbol structId = defaultLocated $ F.symbol structId

structureFieldSymbol :: MonadFlex m => Base.TypeId -> Base.FieldId -> m F.LocSymbol
structureFieldSymbol structId fieldId = defaultLocated $ F.symbol (structId, fieldId)

-- *** Variant

variantDataDecl :: MonadFlex m => Base.Variant Base.Type -> m F.DataDecl
variantDataDecl Base.Variant {..} = do
  ddName <- variantSymbol variantId
  let ddTyCon = F.symbolFTycon ddName
  ddCtors <- forM variantConstructors \(ctorId, paramTypes) -> do
    dcName <- variantConstructorSymbol variantId ctorId
    dcFields <- forM (paramTypes `zip` [0 ..]) \(fieldType, fieldIx) -> do
      dfName <- variantConstructorFieldSymbol variantId ctorId fieldIx
      dfSort <- embedType =<< transType fieldType
      return F.DField {dfName, dfSort}
    return F.DCtor {dcName, dcFields}
  return
    F.DDecl
      { ddTyCon,
        ddVars = 0,
        ddCtors
      }

variantSymbol :: MonadFlex m => Base.TypeId -> m F.LocSymbol
variantSymbol varntId = defaultLocated $ F.symbol varntId

variantConstructorSymbol :: MonadFlex m => Base.TypeId -> Base.TermId -> m F.LocSymbol
variantConstructorSymbol varntId ctorId = defaultLocated $ F.symbol (varntId, ctorId)

variantConstructorFieldSymbol :: MonadFlex m => Base.TypeId -> Base.TermId -> Int -> m F.LocSymbol
variantConstructorFieldSymbol varntId ctorId fieldIx = defaultLocated $ F.symbol (varntId, ctorId, fieldIx)

-- *** Function

functionSymbol :: MonadFlex m => Base.TermId -> m F.LocSymbol
functionSymbol funId = defaultLocated $ F.symbol funId

-- *** Constant

constantSymbol :: MonadFlex m => Base.TermId -> m F.LocSymbol
constantSymbol conId = defaultLocated $ F.symbol conId

-- ** Initializing Refinement Context and Environment

topRefiningCtx :: Base.Module Base.Type Base.Type -> ExceptT RefiningError FlexM RefiningCtx
topRefiningCtx Base.Module {..} = do
  -- !TODO enums, newtypes
  foldrM
    ( \decl ctx -> FlexM.markSection [FlexM.FlexMarkStep (Base.pPrintDeclarationHeader decl) (Just $ pPrint decl)] do
        case decl of
          Base.DeclarationStructure struct@Base.Structure {..} -> do
            FlexM.debug True $ "insert structure" <+> pPrint structureId
            return . (ctx &) $
              ctxStructures . at structureId ?~ struct
          Base.DeclarationNewtype _new -> return ctx -- !TODO
          Base.DeclarationVariant varnt@Base.Variant {..} -> do
            FlexM.debug True $ "insert variant" <+> pPrint variantId
            -- add variant, and each constructor's symId and applicant type
            varnt' <- transType `traverse` varnt

            flip compsM ctx $
              [ return . (ctxVariants . at variantId ?~ varnt),
                runReaderT $ for variantConstructors ask \(ctorId, _paramTypes) m -> do
                  ctorSym <- F.val <$> variantConstructorSymbol variantId ctorId
                  let ctorSymId =
                        SymId
                          { symIdSymbol = ctorSym,
                            symIdMaybeTypeId = Just variantId,
                            symIdMaybeTermId = Just ctorId
                          }
                  paramTypes' <- case lookup ctorId $ Base.variantConstructors varnt' of
                    Just paramTypes' -> return paramTypes'
                    Nothing -> FlexM.throw $ "unknown variant constructor:" <+> pPrint ctorId
                  ctorType <- do
                    return (Base.ApplicantTypeVariantConstructor varnt' ctorId paramTypes')

                  FlexM.debug True $
                    vcat
                      [ "insert variant constructor",
                        nest 2 . vcat $
                          [ "variantId =" <+> pPrint variantId,
                            "ctorId    =" <+> pPrint ctorId,
                            "ctorSymId =" <+> pPrint ctorSymId
                          ]
                      ]

                  comps
                    [ introSymId ctorSymId,
                      introApplicantType ctorSymId ctorType
                    ]
                    m
              ]
          Base.DeclarationEnum _en -> return ctx -- !TODO
          Base.DeclarationAlias _al -> return ctx -- !TODO
          Base.DeclarationFunction fun@Base.Function {..} -> do
            sym <- F.val <$> functionSymbol functionId
            let symId =
                  SymId
                    { symIdSymbol = sym,
                      symIdMaybeTypeId = Nothing,
                      symIdMaybeTermId = Just functionId
                    }
            funType <- do
              functionTypeParameters' <- secondM transType `traverse` functionParameters
              functionTypeOutput' <- transType functionOutput
              return
                Base.FunctionType
                  { functionTypeId = functionId,
                    functionTypeIsTransform = functionIsTransform,
                    functionTypeParameters = functionTypeParameters',
                    functionTypeContextualParameters = functionContextualParameters,
                    functionTypeOutput = functionTypeOutput'
                  }

            FlexM.debug True $
              vcat
                [ "insert function",
                  nest 2 . vcat $
                    [ "functionId =" <+> pPrint functionId,
                      "funType    =" <+> pPrint funType
                    ]
                ]

            flip runReaderT ctx . flip comps ask $
              [ introSymId symId,
                introApplicantType symId (Base.ApplicantTypeFunction funType),
                if functionIsTransform then id else locally ctxFunctions (Map.insert symId fun)
              ]
          Base.DeclarationConstant Base.Constant {..} -> do
            constSym <- F.val <$> constantSymbol constantId
            let constSymId =
                  SymId
                    { symIdSymbol = constSym,
                      symIdMaybeTypeId = Nothing,
                      symIdMaybeTermId = Just constantId
                    }

            constType <- transType constantType

            FlexM.debug True $
              vcat
                [ "insert constant",
                  nest 2 . vcat $
                    [ "constantId =" <+> pPrint constantId,
                      "constType  =" <+> pPrint constType,
                      "constSymId =" <+> pPrint constSymId
                    ]
                ]

            flip runReaderT ctx . flip comps ask $
              [ introSymId constSymId,
                introApplicantType constSymId (Base.ApplicantType constType)
              ]
          Base.DeclarationRefinedType reftTy@Base.RefinedType {..} -> do
            FlexM.debug True $
              vcat
                [ "insert refined type",
                  nest 2 . vcat $
                    [ "refinedTypeId =" <+> pPrint refinedTypeId
                    ]
                ]
            return . (ctx &) $
              ctxRefinedTypes . at refinedTypeId ?~ reftTy
    )
    RefiningCtx
      { _ctxSymIds = mempty,
        _ctxSymbols = mempty,
        _ctxApplicantTypes = mempty,
        _ctxFunctions = mempty,
        _ctxStructures = mempty,
        _ctxRefinedTypes = mempty,
        _ctxRefinedTypes' = mempty,
        _ctxVariants = mempty
      }
    moduleDeclarations

topRefiningEnv :: Base.Module Base.Type Base.Type -> ExceptT RefiningError FlexM RefiningEnv
topRefiningEnv _mdl = do
  return
    RefiningEnv {}

-- ** Liquid Fixpoint Utilities

extractQuants :: MonadFlex m => TypeReft -> m [Quant]
extractQuants tr = do
  let qr = typeAnn tr
  bSort <- embedType tr
  return $
    qreftQuants qr
      <> [ QuantExists
             H.Bind
               { bSym = qreftBind qr,
                 bSort,
                 bPred = H.Reft $ qreftPred qr,
                 bMeta = RefiningError $ "exists:" <+> pPrint tr
               }
         ]

makeBind :: F.Symbol -> F.Sort -> H.Pred -> Bind
makeBind bSym bSort bPred =
  H.Bind
    { bSym,
      bSort,
      bPred,
      bMeta =
        let ppi :: P.PPrint a => a -> Doc
            ppi = pprintInline
         in RefiningError . hsep $ [ppi bSym, ":", ppi bSort, ".", ppi bPred]
    }