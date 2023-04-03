module Language.Flex.Refining.Translating where

import Data.Maybe (fromMaybe)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import qualified Language.Flex.Syntax as Crude
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility

-- - checks for bad forms
transType :: Crude.Type -> RefiningM Type
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
-- - checks for bad forms
transTerm :: Crude.Term Crude.Type -> RefiningM Term
transTerm (Crude.TermLiteral lit ty) = TermLiteral lit <$> transType ty
transTerm term0@(Crude.TermPrimitive prim ty) = transPrimitive prim
  where
    transPrimitive ((Crude.PrimitiveTry te)) = TermPrimitive <$> (PrimitiveTry <$> transTerm te) <*> transType ty
    transPrimitive ((Crude.PrimitiveTuple tes)) = do
      tes' <- transTerm `traverse` tes
      let f te1 te2 = TermPrimitive (PrimitiveTuple te1 te2) (TypeTuple (termType te1) (termType te2))
      case foldl2 f tes' of
        Nothing -> FlexM.throw $ "bad tuple term:" <+> ticks (pPrint term0)
        Just tm' -> return tm'
    transPrimitive ((Crude.PrimitiveArray tes)) = TermPrimitive <$> (PrimitiveArray <$> transTerm `traverse` tes) <*> transType ty
    transPrimitive ((Crude.PrimitiveIf te1 te2 te3)) = TermPrimitive <$> (PrimitiveIf <$> transTerm te1 <*> transTerm te2 <*> transTerm te3) <*> transType ty
    transPrimitive ((Crude.PrimitiveAnd te te')) = TermPrimitive <$> (PrimitiveAnd <$> transTerm te <*> transTerm te') <*> transType ty
    transPrimitive ((Crude.PrimitiveOr te te')) = TermPrimitive <$> (PrimitiveOr <$> transTerm te <*> transTerm te') <*> transType ty
    transPrimitive ((Crude.PrimitiveNot te)) = TermPrimitive <$> (PrimitiveNot <$> transTerm te) <*> transType ty
    transPrimitive ((Crude.PrimitiveEq te te')) = TermPrimitive <$> (PrimitiveEq <$> transTerm te <*> transTerm te') <*> transType ty
    transPrimitive ((Crude.PrimitiveAdd te te')) = TermPrimitive <$> (PrimitiveAdd <$> transTerm te <*> transTerm te') <*> transType ty
    transPrimitive ((Crude.PrimitiveExtends te ti)) = TermPrimitive <$> (PrimitiveExtends <$> transTerm te <*> return ti) <*> transType ty
    transPrimitive ((Crude.PrimitiveCast {})) = FlexM.throw $ "transPrimitive should not encounter this form:" <+> pPrint prim
transTerm (Crude.TermLet tmId te' te2 ty) = TermLet (Just tmId) <$> transTerm te' <*> transTerm te2 <*> transType ty
transTerm (Crude.TermAssert te' te2 ty) = TermAssert <$> transTerm te' <*> transTerm te2 <*> transType ty
transTerm (Crude.TermStructure ti fields ty) = TermStructure ti <$> (transTerm . snd) `traverse` fields <*> transType ty
transTerm (Crude.TermMember te' fi ty) = TermMember <$> transTerm te' <*> return fi <*> transType ty
transTerm (Crude.TermNeutral (Crude.NeutralFunctionApplication ti tes m_tes) ty) = TermApplication ti <$> (transTerm `traverse` (tes <> fromMaybe mempty m_tes)) <*> transType ty
transTerm (Crude.TermNeutral (Crude.NeutralEnumConstruction ti ti') ty) = TermConstructor ti ti' mempty <$> transType ty
transTerm (Crude.TermNeutral (Crude.NeutralVariantConstruction ti ti' tes) ty) = TermConstructor ti ti' <$> transTerm `traverse` tes <*> transType ty
transTerm (Crude.TermNeutral (Crude.NeutralNewtypeConstruction ti _ti' te) ty) = TermStructure ti <$> transTerm `traverse` [te] <*> transType ty
transTerm (Crude.TermNeutral (Crude.Neutral ti) ty) = TermNamed ti <$> transType ty
transTerm (Crude.TermMatch te' _branches ty) = TermMatch <$> transTerm te' <*> error "!TODO correctly interpolate into nested matches and lets" <*> transType ty
transTerm term0@Crude.TermProtoNeutral {} = FlexM.throw $ "transTerm should not encounter this form:" <+> pPrint term0
transTerm term0@Crude.TermAscribe {} = FlexM.throw $ "transTerm should not encounter this form:" <+> pPrint term0

transPattern :: Crude.Pattern Crude.Type -> Term -> Term -> Type -> RefiningM Term
transPattern (Crude.PatternNamed ti _ty) tm1 tm2 ty = return (TermLet (Just ti) tm1 tm2 ty)
transPattern (Crude.PatternDiscard _ty) tm1 tm2 ty = return (TermLet Nothing tm1 tm2 ty)
transPattern (Crude.PatternConstructor {}) _tm1 _tm2 _ty = error "transPattern Crude.PatternConstructor"
