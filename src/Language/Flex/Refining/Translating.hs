module Language.Flex.Refining.Translating where

import Data.Maybe (fromMaybe)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import qualified Language.Flex.Syntax as Crude
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility (bimapM, secondM)

-- - checks for bad forms
transType :: Crude.Type -> RefiningM Type
transType (Crude.TypeNumber nt n) = return $ TypeNumber nt n
transType Crude.TypeBit = return TypeBit
transType Crude.TypeChar = return TypeChar
transType (Crude.TypeArray ty') = TypeArray <$> transType ty'
transType (Crude.TypeTuple tys) = TypeTuple <$> transType `traverse` tys
transType (Crude.TypeOptional ty') = TypeOptional <$> transType ty'
transType (Crude.TypeNamed ti) = return $ TypeNamed ti
transType ty@(Crude.TypeUnifyVar _uv _m_uc) = FlexM.throw $ "transType should not encounter this form:" <+> pPrint ty

-- - inlines functions
-- - homogenizes neutrals
-- - checks for bad forms
transTerm :: Crude.Term Crude.Type -> RefiningM Term
transTerm (Crude.TermLiteral lit ty) = TermLiteral lit <$> transType ty
transTerm (Crude.TermPrimitive prim ty) = TermPrimitive <$> transPrimitive prim <*> transType ty
transTerm (Crude.TermLet pat te' te2 ty) = TermLet <$> transPattern pat <*> transTerm te' <*> transTerm te2 <*> transType ty
transTerm (Crude.TermAssert te' te2 ty) = TermAssert <$> transTerm te' <*> transTerm te2 <*> transType ty
transTerm (Crude.TermStructure ti fields ty) = TermStructure ti <$> (transTerm . snd) `traverse` fields <*> transType ty
transTerm (Crude.TermMember te' fi ty) = TermMember <$> transTerm te' <*> return fi <*> transType ty
transTerm (Crude.TermNeutral (Crude.NeutralFunctionApplication ti tes m_tes) ty) = TermApplication ti <$> (transTerm `traverse` (tes <> fromMaybe mempty m_tes)) <*> transType ty
transTerm (Crude.TermNeutral (Crude.NeutralEnumConstruction ti ti') ty) = TermConstructor ti ti' mempty <$> transType ty
transTerm (Crude.TermNeutral (Crude.NeutralVariantConstruction ti ti' tes) ty) = TermConstructor ti ti' <$> transTerm `traverse` tes <*> transType ty
transTerm (Crude.TermNeutral (Crude.NeutralNewtypeConstruction ti _ti' te) ty) = TermStructure ti <$> transTerm `traverse` [te] <*> transType ty
transTerm (Crude.TermNeutral (Crude.Neutral ti) ty) = TermNamed ti <$> transType ty
transTerm (Crude.TermMatch te' branches ty) = TermMatch <$> transTerm te' <*> (bimapM transPattern transTerm `traverse` branches) <*> transType ty
transTerm term0@Crude.TermProtoNeutral {} = FlexM.throw $ "transTerm should not encounter this form:" <+> pPrint term0
transTerm term0@Crude.TermAscribe {} = FlexM.throw $ "transTerm should not encounter this form:" <+> pPrint term0

transPattern :: Crude.Pattern Crude.Type -> RefiningM Pattern
transPattern (Crude.PatternNamed ti ty) = PatternNamed ti <$> transType ty
transPattern (Crude.PatternDiscard ty) = PatternDiscard <$> transType ty
transPattern (Crude.PatternConstructor {}) = error "transPattern Crude.PatternConstructor"

transPrimitive :: Crude.Primitive Crude.Type -> RefiningM Primitive
transPrimitive (Crude.PrimitiveTry te) = PrimitiveTry <$> transTerm te
transPrimitive (Crude.PrimitiveTuple tes) = PrimitiveTuple <$> transTerm `traverse` tes
transPrimitive (Crude.PrimitiveArray tes) = PrimitiveArray <$> transTerm `traverse` tes
transPrimitive (Crude.PrimitiveIf te te' te2) = PrimitiveIf <$> transTerm te <*> transTerm te' <*> transTerm te2
transPrimitive (Crude.PrimitiveAnd te te') = PrimitiveAnd <$> transTerm te <*> transTerm te'
transPrimitive (Crude.PrimitiveOr te te') = PrimitiveOr <$> transTerm te <*> transTerm te'
transPrimitive (Crude.PrimitiveNot te) = PrimitiveNot <$> transTerm te
transPrimitive (Crude.PrimitiveEq te te') = PrimitiveEq <$> transTerm te <*> transTerm te'
transPrimitive (Crude.PrimitiveAdd te te') = PrimitiveAdd <$> transTerm te <*> transTerm te'
transPrimitive (Crude.PrimitiveExtends te ti) = PrimitiveExtends <$> transTerm te <*> return ti
transPrimitive prim@(Crude.PrimitiveCast {}) = FlexM.throw $ "transPrimitive should not encounter this form:" <+> pPrint prim
