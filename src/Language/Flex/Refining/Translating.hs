module Language.Flex.Refining.Translating where

import Control.Lens (At (at), locally, to, (^.), _3)
import Control.Monad (filterM, foldM, forM, void, when)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (foldlM)
import Data.Functor
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Language.Fixpoint.Types as F
import qualified Language.Flex.FlexBug as FlexBug
import Language.Flex.FlexM (FlexM, freshSymbol)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Embedding (embedTerm, embedType)
import Language.Flex.Refining.RefiningM (RefiningM, ctxBindings, ctxSymbols, freshSymId, freshSymIdTermId, freshenBind, freshenTermId, getApplicantType, getFunction, getStructure, getSymId, introApplicantType, introBinding, introSymId, liftFlexM_RefiningM, throwRefiningError)
import Language.Flex.Refining.Syntax
import Language.Flex.Syntax (Literal (..), renameTerm)
import qualified Language.Flex.Syntax as Base
import Text.PrettyPrint.HughesPJClass
import Utility

-- ** Translate Term

transTerm :: Base.Term Base.Type -> RefiningM (Term Base.Type)
transTerm term = do
  FlexM.debug . FlexM.FlexLog "refining" $ "[transTerm]" <+> pPrint term
  case term of
    Base.TermLiteral lit ty -> return $ TermLiteral lit ty
    Base.TermPrimitive prim ty ->
      case prim of
        Base.PrimitiveTry te -> TermPrimitive <$> (PrimitiveTry <$> transTerm te) <*> return ty
        Base.PrimitiveTuple tes | length tes < 2 -> FlexBug.throw $ FlexM.FlexLog "refining" "attempted to transTerm on a Base.PrimitiveTuple that has length terms < 2"
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
        Base.PrimitiveCast _ -> FlexBug.throw $ FlexM.FlexLog "refining" $ "PrimitiveCast should not appear in typed term:" <+> pPrint term
    -- local binding is added to refinement context during refinement checking,
    -- not translation (since the implementation of the let needs to be checked
    -- first)
    Base.TermLet {termPattern, termTerm, termBody} -> do
      symId <- case termPattern of
        Base.PatternNamed ti _ty -> freshSymIdTermId ti
        Base.PatternDiscard _ty -> freshSymId "discard"
      tm <- transTerm termTerm
      ty <- liftFlexM_RefiningM $ transType $ termAnn tm
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
      return
        TermStructure
          { termStructureId,
            termFields = fields',
            termAnn
          }
    Base.TermMember _te _fi _ty -> error "transTerm"
    Base.TermNeutral app mb_args mb_cxargs ty -> do
      symId <- getSymId (void app)
      -- note that we avoid inserting the globally-known refinement types here,
      -- since we will do that in the refining step anyway, and we must provide a
      -- `Base.Type` right now anyway since we're producing a `Term Base.Type`
      getApplicantType symId >>= \case
        -- Function application is inlined. For example, given
        --
        -- @
        --    function f(x: bit) -> bit {
        --      let y = !x;
        --      y
        --    }
        -- @
        --
        -- then the application @f(true)@ is inlined to be
        --
        -- @
        --    let x' = true;
        --    let y' = !x';
        --    y'
        -- @
        --
        -- where @x'@ and @y'@ are fresh variables substituted in for @x@
        -- and @y@
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
                _ -> FlexBug.throw $ FlexM.FlexLog "refining" $ "function type's contextual parameters doesn't correspond to application's contextual arguments: " <+> pPrint functionContextualParameters <+> "," <+> pPrint mb_cxargs

            -- argId => (argSymId, tm)
            let freshening = Map.union fresheningArgs fresheningCxargs

            -- argId => argSymId
            let renaming = fst <$> freshening

            -- rename via `renaming` in `functionBody`
            let functionBody' :: Base.Term Base.Type
                functionBody' =
                  comps
                    ( Map.elems freshening <&> \(argSymId, arg') tm ->
                        Base.TermLet (Base.PatternNamed argSymId (Base.termAnn arg')) arg' tm (Base.termAnn tm)
                    )
                    $ renameTerm renaming functionBody

            transTerm functionBody'
        _ -> do
          mb_args' <- (transTerm `traverse`) `traverse` mb_args
          mb_cxargs' <- (transTerm `traverse`) `traverse` mb_cxargs
          let args'' = fromMaybe [] mb_args' ++ fromMaybe [] mb_cxargs'
          return $ TermNeutral symId args'' ty
    Base.TermMatch _te _x0 _ty -> error "transTerm"
    -- invalid
    Base.TermAscribe _te _ty _ty' -> FlexBug.throw $ FlexM.FlexLog "refining" $ "term ascribe should not appear in typed term:" <+> pPrint term

transType :: Base.Type -> FlexM TypeReft
transType type_ = case type_ of
  Base.TypeNumber numty n -> do
    x <- freshSymbol (render $ pPrint type_)
    let p = case numty of
          Base.TypeInt ->
            -- -2^(n-1) < x < 2^(n-1)
            F.conj
              [ F.PAtom F.Le (F.expr (-(2 ^ (n - 1)) :: Int)) (F.expr x),
                F.PAtom F.Lt (F.expr x) (F.expr (2 ^ (n - 1) :: Int))
              ]
          Base.TypeUInt ->
            -- 0 <= x < 2^n
            F.conj
              [ F.PAtom F.Le (F.expr (0 :: Int)) (F.expr x),
                F.PAtom F.Lt (F.expr x) (F.expr (2 ^ n :: Int))
              ]
          Base.TypeFloat -> error "TODO: transType TypeFloat"
    let atomic = case numty of
          Base.TypeInt -> TypeInt
          Base.TypeUInt -> TypeInt
          Base.TypeFloat -> TypeFloat
    return $ TypeAtomic atomic (F.reft x p)
  Base.TypeBit -> return $ TypeAtomic TypeBit F.trueReft
  Base.TypeChar -> return $ TypeAtomic TypeChar F.trueReft
  Base.TypeArray Base.TypeChar -> return $ TypeAtomic TypeString F.trueReft
  Base.TypeArray _ty -> error "transType TODO"
  Base.TypeTuple tys -> typeTuple =<< transType `traverse` tys
  Base.TypeOptional _ty -> error "transType TODO"
  Base.TypeNamed _ti -> error "transType TODO"
  Base.TypeStructure Base.Structure {structureId = structId} -> do
    {-
    Structure {..} <- getStructure structId

    symStruct <- freshSymbol "struct"

    let tyStruct = TypeStructure structureId ()

    -- pEq: struct = S x1 ... xN
    pEq <-
      eqPred
        (termVar (fromSymbolToSymId symStruct) tyStruct)
        ( TermStructure
            structureId
            ( structureFields <&> \(fieldId, ty) ->
                ( fieldId,
                  termVar
                    (fromSymbolToSymId (F.symbol (structureId, fieldId)))
                    (void ty)
                )
            )
            tyStruct
        )

    -- pUser
    let pUser = structureRefinement

    tys <- (liftFlexM_RefiningM . embedType . snd) `traverse` structureFields

    -- p: exists x1 ... xN . pEq(x1, ..., xN) && pUser(x1, ..., xN)
    let p =
          F.pExist
            ( (structureFields `zip` tys)
                <&> \((fieldId, _tyField), srtField) ->
                  (F.symbol (structureId, fieldId), srtField)
            )
            $ F.conj [pEq, pUser]

    -- r: { struct: S | p(struct) }
    let r = F.reft symStruct p

    return
      TypeStructure
        { typeStructureId = structureId,
          typeAnn = r
        }
    -}
    error "TODO: transType TypeStructure"
  Base.TypeEnum _en -> error "transType TODO"
  Base.TypeVariant _vari -> error "transType TODO"
  Base.TypeNewtype _new -> error "transType TODO"
  -- invalid
  Base.TypeUnifyVar _ _ -> FlexBug.throw $ FlexM.FlexLog "refining" $ "type unification variable should not appear in normalized type:" <+> pPrint type_

-- ** Basic Types

-- | Refined structure type.
typeStructure :: Base.Structure -> [(Base.FieldId, TypeReft)] -> RefiningM TypeReft
typeStructure struct@Base.Structure {..} fieldTys = do
  symStruct <- liftFlexM_RefiningM $ freshSymbol "struct"

  -- tyStruct: S a1 ... aN
  let tyStruct = TypeStructure struct (second void <$> fieldTys) ()

  -- p1(struct, x1, ..., xN): p1(x1, ..., xN): struct = S a1 ... aN
  p1 <-
    liftFlexM_RefiningM $
      eqPred
        (termVar (fromSymbolToSymId symStruct) tyStruct)
        ( TermStructure
            structureId
            ( fieldTys <&> \(fieldId, ty) ->
                ( fieldId,
                  fromSymbolToTerm (F.reftBind (typeAnn ty)) (void ty)
                )
            )
            tyStruct
        )

  -- p2(x1, ..., xN): r1(x1) && ... && rN(xN)
  let p2 = F.conj $ fieldTys <&> \(_, ty) -> F.reftPred $ typeAnn ty

  -- p(struct): exists x1, ..., xN . p1(struct, x1, ..., xN) && p2(x1, ..., xN)
  fieldSrts <- secondM (liftFlexM_RefiningM . embedType) `traverse` fieldTys
  let p =
        F.pExist
          ( fieldSrts <&> \(fieldId, srt) ->
              (F.symbol (structureId, fieldId), srt)
          )
          $ F.conj [p1, p2]

  -- r: { struct: S a1 ... aN | p(struct) }
  let r = F.reft symStruct p

  return $ TypeStructure struct fieldTys r

-- | Refined tuple type.
--
-- > typeTuple [.., { xI: aI | pI(xI) }, ...] = { tuple: (..., aI, ...) | (tuple
-- > == (..., xI, ....)) && ... && pI(xI) && .... }
typeTuple :: [TypeReft] -> FlexM TypeReft
typeTuple tys_ = do
  let go :: TypeReft -> TypeReft -> FlexM TypeReft
      go ty1 ty2 = do
        -- ty1: { x1: a1 | r1(x1) }
        -- ty2: { x2: a2 | r2(x2) }

        -- r1(x1)
        -- r2(x2)
        let r1 = typeAnn ty1
            r2 = typeAnn ty2

        symTuple <- freshSymbol "tuple"

        -- tyTuple: (ty1, ty2)
        -- unrefined, since only used for embedding
        let tyTuple = TypeTuple (void ty1, void ty2) ()

        -- p1(tuple, x1, x2): tuple == (x1, x2)
        p1 <-
          eqPred
            (termVar (fromSymbolToSymId symTuple) tyTuple)
            ( TermPrimitive
                ( PrimitiveTuple
                    ( fromSymbolToTerm (F.reftBind r1) (void ty1),
                      fromSymbolToTerm (F.reftBind r2) (void ty2)
                    )
                )
                tyTuple
            )

        -- p2(tuple, x1, x2): r1(x1) && r2(x2)
        let p2 = F.conj $ [ty1, ty2] <&> (F.reftPred . typeAnn)

        -- r: { tuple: tyTuple | exists x1 x2 . p1(y1, x2) && p2(y1, x2) }
        srt1 <- embedType ty1
        srt2 <- embedType ty2
        let r =
              F.reft symTuple $
                F.pExist [(F.reftBind r1, srt1), (F.reftBind r2, srt2)] $
                  F.conj [p1, p2]

        -- { tuple: (a1, a2) | p1 && p2 }
        return $ TypeTuple (ty1, ty2) r

  case tys_ of
    [] -> error "typeTuple []"
    [_] -> error "typeTuple [ _ ]"
    (ty : tys) -> foldlM go ty tys -- TUPLE: fold left

-- ** Utilities

-- | The predicate that asserts that two (embedded) terms are equal.
--
-- > eqPred tm1 tm2 = { tm1 == tm2 }
eqPred :: Term (Type ()) -> Term (Type ()) -> FlexM F.Pred
eqPred tm1 tm2 =
  embedTerm $
    TermPrimitive
      (PrimitiveEq tm1 tm2)
      (typeBit ())

-- *** Translating to Sorts

-- -- TODO:DEPRECATED: is this still needed?
-- sortOfBaseType :: Base.Type -> F.Sort
-- sortOfBaseType = \case
--   Base.TypeNumber nt _n -> case nt of
--     Base.TypeInt -> F.intSort
--     Base.TypeUInt -> F.intSort
--     Base.TypeFloat -> F.realSort
--   Base.TypeBit -> F.boolSort
--   Base.TypeChar -> F.charSort
--   -- TODO: use FApp (type constructor application) and FObj (uninterpreted
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
