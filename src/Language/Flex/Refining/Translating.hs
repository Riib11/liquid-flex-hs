module Language.Flex.Refining.Translating where

import Control.Lens
import Control.Monad (foldM, liftM2)
import Control.Monad.State (modify)
import Data.Functor
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Traversable
import Language.Flex.FlexM (FlexM)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import qualified Language.Flex.Syntax as Crude
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility hiding (for)

-- !TODO never need to look up local information during translation, so no need
-- to put things into context (e.g. bindings at matches and lets)

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
transType ty@(Crude.TypeUnifyVar _uv) = FlexM.throw $ "transType should not encounter this form:" <+> pPrint ty

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
    transPrimitive Crude.PrimitiveException = return $ TermPrimitive PrimitiveException ty
    transPrimitive (Crude.PrimitiveTry te) = TermPrimitive <$> (PrimitiveTry <$> transTerm te) <*> return ty
    transPrimitive Crude.PrimitiveNone = return $ TermPrimitive PrimitiveNone ty
    transPrimitive (Crude.PrimitiveSome tm) = TermPrimitive <$> (PrimitiveSome <$> transTerm tm) <*> return ty
    transPrimitive (Crude.PrimitiveTuple tes) = do
      tes' <- transTerm `traverse` tes
      let f te1 te2 = TermPrimitive (PrimitiveTuple te1 te2) (TypeTuple (termType te1) (termType te2))
      case foldl2 f tes' of
        Nothing -> FlexM.throw $ "bad tuple term:" <+> ticks (pPrint term0)
        Just tm' -> return tm'
    transPrimitive (Crude.PrimitiveArray tes) = TermPrimitive <$> (PrimitiveArray <$> transTerm `traverse` tes) <*> return ty
    transPrimitive (Crude.PrimitiveIf te1 te2 te3) = TermPrimitive <$> (PrimitiveIf <$> transTerm te1 <*> transTerm te2 <*> transTerm te3) <*> return ty
    transPrimitive (Crude.PrimitiveNot te) = TermPrimitive <$> (PrimitiveNot <$> transTerm te) <*> return ty
    transPrimitive (Crude.PrimitiveEq b te te') = TermPrimitive <$> (PrimitiveEq b <$> transTerm te <*> transTerm te') <*> return ty
    transPrimitive (Crude.PrimitiveBoolBinOp bbo te te') = TermPrimitive <$> (PrimitiveBoolBinOp bbo <$> transTerm te <*> transTerm te') <*> return ty
    transPrimitive (Crude.PrimitiveNumBinOp nbo te te') = TermPrimitive <$> (PrimitiveNumBinOp nbo <$> transTerm te <*> transTerm te') <*> return ty
    transPrimitive (Crude.PrimitiveNumBinRel nbr te te') = TermPrimitive <$> (PrimitiveNumBinRel nbr <$> transTerm te <*> transTerm te') <*> return ty
    -- !TODO is there something special to do here, by introducing global facts
    -- or something about extension relations?
    transPrimitive (Crude.PrimitiveExtends te ti) = TermPrimitive <$> (PrimitiveExtends <$> transTerm te <*> return ti) <*> return ty
    -- unwrap total casts; keep partial casts
    transPrimitive (Crude.PrimitiveCast te) = do
      let tyInner = Crude.termAnn te
      let tyOuter = ty
      isTotalCast tyInner tyOuter >>= \case
        True -> transTerm te
        False -> do
          FlexM.debug True $ "transTerm': got here 1"
          envUsedCastings %= Set.insert (tyInner, tyOuter)
          TermPrimitive <$> (PrimitiveCast <$> transTerm te <*> pure tyInner <*> pure tyOuter) <*> pure tyOuter
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
transTerm' (Crude.TermMatch tm branches ty) = do
  tm' <- transTerm tm
  branches' <-
    branches <&*> \(pat, body) -> case pat of
      (Crude.PatternConstructor varntId ctorId tmIds _ty) -> (PatternConstructor varntId ctorId tmIds,) <$> transTerm body
      (Crude.PatternSome tmId _ty) -> (PatternSome tmId,) <$> transTerm body
      (Crude.PatternNone _ty) -> (PatternNone,) <$> transTerm body
  return $ TermMatch tm' branches' ty
transTerm' term0@Crude.TermProtoNeutral {} = FlexM.throw $ "transTerm should not encounter this form:" <+> pPrint term0
transTerm' term0@Crude.TermAscribe {} = FlexM.throw $ "transTerm should not encounter this form:" <+> pPrint term0

-- Is a cast from ty1 to ty2 total?
isTotalCast :: Type -> Type -> RefiningM Bool
-- TypeUInt ~~> _
isTotalCast (TypeNumber Crude.TypeUInt n) (TypeNumber Crude.TypeUInt n') = return (n <= n')
isTotalCast (TypeNumber Crude.TypeUInt n) (TypeNumber Crude.TypeInt n') = return (n <= n')
isTotalCast (TypeNumber Crude.TypeUInt n) (TypeNumber Crude.TypeFloat n') = return (n <= n')
-- TypeInt ~~> _
isTotalCast (TypeNumber Crude.TypeInt _n) (TypeNumber Crude.TypeUInt _n') = return False
isTotalCast (TypeNumber Crude.TypeInt n) (TypeNumber Crude.TypeInt n') = return (n <= n')
isTotalCast (TypeNumber Crude.TypeInt n) (TypeNumber Crude.TypeFloat n') = return (n <= n')
-- TypeFloat ~~> _
isTotalCast (TypeNumber Crude.TypeFloat _n) (TypeNumber Crude.TypeUInt _n') = return False
isTotalCast (TypeNumber Crude.TypeFloat _n) (TypeNumber Crude.TypeInt _n') = return False
isTotalCast (TypeNumber Crude.TypeFloat n) (TypeNumber Crude.TypeFloat n') = return (n <= n')
isTotalCast TypeBit TypeBit = return True
isTotalCast TypeChar TypeChar = return True
isTotalCast (TypeArray ty) (TypeArray ty') = isTotalCast ty ty'
isTotalCast (TypeTuple ty1 ty2) (TypeTuple ty1' ty2') = liftM2 (&&) (isTotalCast ty1 ty1') (isTotalCast ty2 ty2')
isTotalCast (TypeOptional ty) (TypeOptional ty') = isTotalCast ty ty'
-- !TODO handle structure extension
isTotalCast (TypeNamed tyId) (TypeNamed tyId') = return (tyId == tyId')
isTotalCast _ _ = return False

-- transBranch :: Term -> Crude.Branch Type -> RefiningM Branch
-- transBranch _tm (Crude.PatternConstructor ti ti' tis _ty, body) = do
--   let pat' = PatternConstructor ti ti' tis
--   body' <- transTerm body
--   return (pat', body')
-- transBranch _tm (Crude.PatternSome ti _ty, body) = do
--   let pat' = PatternSome ti
--   body' <- transTerm body
--   return (pat', body')
-- transBranch _tm (Crude.PatternNone _ty, body) = do
--   let pat' = PatternNone
--   body' <- transTerm body
--   return (pat', body')

transRefinement :: Crude.Refinement Type -> RefiningM Term
transRefinement (Crude.Refinement tm) = transTerm tm
