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
-- - translates PrimitiveArray to TermConstructor
-- - translates PrimitiveTuple to TermConstructor
-- - checks for bad forms
transTerm :: Crude.Term Type -> RefiningM Term
transTerm (Crude.TermLiteral lit ty) = return $ TermLiteral lit ty
transTerm term0@(Crude.TermPrimitive prim ty) = transPrimitive prim
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
    transPrimitive ((Crude.PrimitiveExtends te ti)) = TermPrimitive <$> (PrimitiveExtends <$> transTerm te <*> return ti) <*> return ty
    transPrimitive ((Crude.PrimitiveCast {})) = FlexM.throw $ "transPrimitive should not encounter this form:" <+> pPrint prim
transTerm (Crude.TermLet tmId te' te2 ty) = TermLet (Just tmId) <$> transTerm te' <*> transTerm te2 <*> return ty
transTerm (Crude.TermAssert te' te2 ty) = TermAssert <$> transTerm te' <*> transTerm te2 <*> return ty
transTerm (Crude.TermStructure ti fields ty) = TermStructure ti <$> secondM transTerm `traverse` fields <*> return ty
transTerm (Crude.TermMember tm fieldId ty) = do
  -- s.x_i ~~> (match s with S x_1 ... x_n => x_i)
  tm' <- transTerm tm
  Structure {..} <- case termType tm' of
    TypeNamed structId -> lookupStructure structId
    type_ -> FlexM.throw $ "expected" <+> ticks (pPrint tm') <+> "to have a structure type (TypeNamed), but instead it has type" <+> ticks (pPrint type_)
  let fieldIds = structureFields <&> Crude.fromFieldIdToTermId . fst
  let pat = PatternConstructor structureId structureConstructorId fieldIds
  return $ TermMatch tm' [(pat, TermNamed (Crude.fromFieldIdToTermId fieldId) ty)] ty
transTerm (Crude.TermNeutral (Crude.NeutralFunctionApplication funId args mb_cxargs) ty) = do
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
              Crude.TermLet paramId argTerm body'' (Crude.termAnn body'')
      -- translate new body
      transTerm body'
transTerm (Crude.TermNeutral (Crude.NeutralEnumConstruction ti ti') ty) = return $ TermConstructor ti ti' mempty ty
transTerm (Crude.TermNeutral (Crude.NeutralVariantConstruction ti ti' tes) ty) = TermConstructor ti ti' <$> transTerm `traverse` tes <*> return ty
transTerm (Crude.TermNeutral (Crude.NeutralNewtypeConstruction ti _ti te) ty) = do
  TermStructure ti <$> secondM transTerm `traverse` [(error "!TODO get the field of the newtype", te)] <*> return ty
transTerm (Crude.TermNeutral (Crude.Neutral ti) ty) = return $ TermNamed ti ty
transTerm (Crude.TermMatch te' _branches ty) = TermMatch <$> transTerm te' <*> error "!TODO correctly interpolate into nested matches and lets" <*> return ty
transTerm term0@Crude.TermProtoNeutral {} = FlexM.throw $ "transTerm should not encounter this form:" <+> pPrint term0
transTerm term0@Crude.TermAscribe {} = FlexM.throw $ "transTerm should not encounter this form:" <+> pPrint term0

transPattern :: Crude.Pattern Crude.Type -> Term -> Term -> Type -> FlexM Term
transPattern (Crude.PatternNamed ti _ty) tm1 tm2 ty = return (TermLet (Just ti) tm1 tm2 ty)
transPattern (Crude.PatternDiscard _ty) tm1 tm2 ty = return (TermLet Nothing tm1 tm2 ty)
transPattern (Crude.PatternConstructor {}) _tm1 _tm2 _ty = error "transPattern Crude.PatternConstructor"

transRefinement :: Crude.Refinement Type -> RefiningM Term
transRefinement (Crude.Refinement tm) = transTerm tm
