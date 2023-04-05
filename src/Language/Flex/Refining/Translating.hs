module Language.Flex.Refining.Translating where

import Data.Functor
import qualified Data.Map as Map
import Data.Maybe
import Data.Traversable
import Language.Flex.FlexM (FlexM)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import qualified Language.Flex.Syntax as Crude
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility hiding (for)

-- - checks for bad forms
transType :: Crude.Type -> FlexM Type
transType (Crude.TypeNumber nt n) = return $ TypeNumber nt n
transType Crude.TypeBit = return TypeBit
transType Crude.TypeChar = return TypeChar
transType (Crude.TypeArray ty') = TypeArray <$> transType ty'
transType type0@(Crude.TypeTuple tys) = do
  tys' <- transType `traverse` tys
  case foldl2 TypeTuple tys' of
    Nothing -> FlexM.throw $ "bad tuple type:" <+> ticks (pPrint type0)
    Just ty' -> return ty'
transType (Crude.TypeOptional ty') = TypeOptional <$> transType ty'
transType (Crude.TypeNamed ti) = return $ TypeNamed ti
transType ty@(Crude.TypeUnifyVar _uv _m_uc) = FlexM.throw $ "transType should not encounter this form:" <+> pPrint ty

-- - inlines functions
-- - homogenizes neutrals
-- - translates TermMember to TermMatch
-- - translates PrimitiveArray to TermConstructor -- !TODO don't do this, instead use built-in array datatype
-- - translates PrimitiveTuple to TermConstructor
-- - checks for bad forms
transTerm :: Crude.Term Type -> RefiningM Term
transTerm term = FlexM.markSectionResult (FlexM.FlexMarkStep ("transTerm:" <+> pPrint term) Nothing) pPrint term pPrint do
  transTerm' term

transTerm' :: Crude.Term Type -> RefiningM Term
transTerm' (Crude.TermLiteral lit ty) = do
  FlexM.debug True $ "lit =" <+> pPrint lit
  FlexM.debug True $ "ty  =" <+> pPrint ty
  return $ TermLiteral lit ty
transTerm' term0@(Crude.TermPrimitive prim ty) = transPrimitive prim
  where
    transPrimitive ((Crude.PrimitiveTry te)) = TermPrimitive <$> (PrimitiveTry <$> transTerm te) <*> return ty
    transPrimitive ((Crude.PrimitiveTuple tes)) = do
      tes' <- transTerm `traverse` tes
      let f te1 te2 = TermPrimitive (PrimitiveTuple te1 te2) (TypeTuple (termType te1) (termType te2))
      case foldl2 f tes' of
        Nothing -> FlexM.throw $ "bad tuple term:" <+> ticks (pPrint term0)
        Just tm' -> return tm'
    transPrimitive ((Crude.PrimitiveArray tes)) = TermPrimitive <$> (PrimitiveArray <$> transTerm `traverse` tes) <*> return ty
    transPrimitive ((Crude.PrimitiveIf te1 te2 te3)) = TermPrimitive <$> (PrimitiveIf <$> transTerm te1 <*> transTerm te2 <*> transTerm te3) <*> return ty
    transPrimitive ((Crude.PrimitiveAnd te te')) = TermPrimitive <$> (PrimitiveAnd <$> transTerm te <*> transTerm te') <*> return ty
    transPrimitive ((Crude.PrimitiveOr te te')) = TermPrimitive <$> (PrimitiveOr <$> transTerm te <*> transTerm te') <*> return ty
    transPrimitive ((Crude.PrimitiveNot te)) = TermPrimitive <$> (PrimitiveNot <$> transTerm te) <*> return ty
    transPrimitive ((Crude.PrimitiveEq te te')) = TermPrimitive <$> (PrimitiveEq <$> transTerm te <*> transTerm te') <*> return ty
    transPrimitive ((Crude.PrimitiveAdd te te')) = TermPrimitive <$> (PrimitiveAdd <$> transTerm te <*> transTerm te') <*> return ty
    -- !TODO is there something special to do here, by introducing global facts
    -- or something about extension relations?
    transPrimitive ((Crude.PrimitiveExtends te ti)) = TermPrimitive <$> (PrimitiveExtends <$> transTerm te <*> return ti) <*> return ty
    transPrimitive ((Crude.PrimitiveCast {})) = FlexM.throw $ "transPrimitive should not encounter this form:" <+> pPrint prim
transTerm' (Crude.TermLet mb_tmId te' te2 ty) = TermLet mb_tmId <$> transTerm te' <*> transTerm te2 <*> return ty
transTerm' (Crude.TermAssert te' te2 ty) = TermAssert <$> transTerm te' <*> transTerm te2 <*> return ty
transTerm' (Crude.TermStructure structId fields ty) = do
  fields' <- fields <&*> \(_fieldId, fieldTerm) -> transTerm fieldTerm
  return $ TermStructure structId fields' ty
-- use field accessor F.symbol (structId, fieldId)
transTerm' term0@(Crude.TermMember tm fieldId ty) = do
  structId <- case Crude.termAnn tm of
    TypeNamed structId -> return structId
    _ -> FlexM.throw $ "expected term TermMember to have type TypeNamed, but instead have" <+> ticks (pPrint term0 <+> ":" <+> pPrint ty)
  tm' <- transTerm tm
  return $ TermMember structId tm' fieldId ty
transTerm' (Crude.TermNeutral (Crude.NeutralFunctionApplication funId args mb_cxargs) ty) = do
  let args' = args <> fromMaybe mempty mb_cxargs
  Function {..} <- lookupFunction funId
  if functionIsTransform
    then do
      -- a transform application is treated as an uninterpreted function appliction
      TermApplication funId <$> (transTerm `traverse` args') <*> return ty
    else do
      -- a function application is inlined
      -- freshen parameter ids
      params' <- functionParameters <&*> FlexM.freshenTermId . fst
      -- rename original ids to fresh ids in function body
      let rho = Map.fromList $ (fst <$> functionParameters) `zip` params'
      let body = Crude.renameTerm rho functionBody
      -- wrap the body in a `TermLet` for each function parameter that now has
      -- an assigned value
      let body' =
            foldr321 (params' `zip` args) body \(paramId, argTerm) body'' ->
              Crude.TermLet (Just paramId) argTerm body'' (Crude.termAnn body'')
      -- translate new body
      transTerm body'
transTerm' (Crude.TermNeutral (Crude.NeutralEnumConstruction newtyId ctorId) ty) = return $ TermConstructor newtyId ctorId mempty ty
transTerm' (Crude.TermNeutral (Crude.NeutralVariantConstruction varntId ctorId tes) ty) = TermConstructor varntId ctorId <$> transTerm `traverse` tes <*> return ty
transTerm' (Crude.TermNeutral (Crude.NeutralNewtypeConstruction newtyId _ctorId tm) ty) = do
  tm' <- transTerm tm
  return $ TermStructure newtyId [tm'] ty
transTerm' (Crude.TermNeutral (Crude.Neutral ti) ty) = return $ TermNamed ti ty
transTerm' (Crude.TermMatch te' _branches ty) = TermMatch <$> transTerm te' <*> error "!TODO interpolate into nested matches and lets" <*> return ty
transTerm' term0@Crude.TermProtoNeutral {} = FlexM.throw $ "transTerm should not encounter this form:" <+> pPrint term0
transTerm' term0@Crude.TermAscribe {} = FlexM.throw $ "transTerm should not encounter this form:" <+> pPrint term0

transPattern :: Crude.Pattern Crude.Type -> Term -> Term -> Type -> FlexM Term
transPattern (Crude.PatternNamed ti _ty) tm1 tm2 ty = return (TermLet (Just ti) tm1 tm2 ty)
transPattern (Crude.PatternDiscard _ty) tm1 tm2 ty = return (TermLet Nothing tm1 tm2 ty)
transPattern (Crude.PatternConstructor {}) _tm1 _tm2 _ty = error "transPattern Crude.PatternConstructor"

transRefinement :: Crude.Refinement Type -> RefiningM Term
transRefinement (Crude.Refinement tm) = transTerm tm
