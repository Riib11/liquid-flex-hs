module Language.Flex.Refining.Reflecting where

import Control.Category hiding ((.))
import Control.Monad (forM)
import Data.Functor
import Data.Text (pack)
import qualified Data.Text as Text
import qualified Language.Fixpoint.Types as F
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Primitive
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import qualified Language.Flex.Syntax as Crude
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility

reflTerm :: Term -> RefiningM F.Expr
reflTerm (TermLiteral lit _ty) = reflLiteral lit
reflTerm (TermPrimitive prim _ty) = reflPrimitive prim
reflTerm (TermLet Nothing _tm1 tm2 _ty) = reflTerm tm2
reflTerm (TermLet (Just x) tm1 tm2 _ty) = do
  -- (let x = a in b) ~~> ((lam x => b) a)
  ex1 <- reflTerm tm1
  srt <- reflType (termType tm1)
  ex2 <- reflTerm tm2
  return $ F.eApps (F.ELam (F.symbol (makeTermIdSymbol x), srt) ex2) [ex1]
reflTerm (TermAssert _te tm _ty) = do
  -- assert is unwrapped
  reflTerm tm
reflTerm (TermNamed x _) = do
  return $ F.eVar (makeTermIdSymbol x)
reflTerm (TermApplication f tms _ty) = do
  exs <- reflTerm `traverse` tms
  return $ F.eApps (F.eVar (makeTermIdSymbol f)) exs
reflTerm (TermStructure structId fields _ty) = do
  exs <- reflTerm <$*> fields
  return $ F.eApps (F.eVar (makeStructureConstructorSymbol structId)) exs
reflTerm (TermMember structId tm fieldId _ty) = do
  ex <- reflTerm tm
  -- use the field accessor defined by the datatype
  return $ F.eApps (F.eVar (makeStructureFieldAccessorSymbol (structId, fieldId))) [ex]
reflTerm (TermConstructor varntId ctorId args _ty) = do
  exs <- reflTerm `traverse` args
  return $ F.eApps (F.eVar (makeVariantConstructorSymbol (varntId, ctorId))) exs
-- !TODO use field accessors
reflTerm (TermMatch _tm _branches _ty) = error "reflTerm TermMatch"

reflPrimitive :: Primitive -> RefiningM F.Expr
reflPrimitive (PrimitiveTry {}) = error "!TODO reflect PrimitiveTry"
reflPrimitive PrimitiveNone = return $ F.eVar optional_NoneConstructorSymbol
reflPrimitive (PrimitiveSome tm) = do
  ex <- reflTerm tm
  return $ F.eApps (F.eVar optional_SomeConstructorSymbol) [ex]
reflPrimitive (PrimitiveTuple tm1 tm2) = do
  exs <- reflTerm `traverse` [tm1, tm2]
  return $ F.eApps (F.eVar tuple_TupleConstructorSymbol) exs
reflPrimitive (PrimitiveArray _tms) = do
  -- exs <- reflTerm `traverse` tms
  -- case exs of
  --   -- Nil
  --   [] -> return $ F.eVar array_ConsConstructorSymbol
  --   -- Cons _ _
  --   firstExpr : restExprs -> return $
  --     for restExprs firstExpr \headExpr tailExpr ->
  --       F.eApps (F.eVar array_ConsConstructorSymbol) [headExpr, tailExpr]
  error "use LH's built-in arrays"
reflPrimitive (PrimitiveIf tm1 tm2 tm3) = F.EIte <$> reflTerm tm1 <*> reflTerm tm2 <*> reflTerm tm3
reflPrimitive (PrimitiveAnd tm1 tm2) = F.pAnd <$> reflTerm `traverse` [tm1, tm2]
reflPrimitive (PrimitiveOr tm1 tm2) = F.pOr <$> reflTerm `traverse` [tm1, tm2]
reflPrimitive (PrimitiveNot tm) = F.PNot <$> reflTerm tm
reflPrimitive (PrimitiveEq tm1 tm2) = F.PAtom F.Eq <$> reflTerm tm1 <*> reflTerm tm2
reflPrimitive (PrimitiveAdd tm1 tm2) = F.EBin F.Plus <$> reflTerm tm1 <*> reflTerm tm2
reflPrimitive (PrimitiveLe tm1 tm2) = F.PAtom F.Le <$> reflTerm tm1 <*> reflTerm tm2
reflPrimitive (PrimitiveLt tm1 tm2) = F.PAtom F.Lt <$> reflTerm tm1 <*> reflTerm tm2
reflPrimitive (PrimitiveFirst tm) = F.eApps (F.eVar tuple_FirstFieldAccessorSymbol) <$> reflTerm `traverse` [tm]
reflPrimitive (PrimitiveSecond tm) = F.eApps (F.eVar tuple_SecondFieldAccessorSymbol) <$> reflTerm `traverse` [tm]
reflPrimitive (PrimitiveExtends {}) = error "!TODO reflect PrimitiveExtends"

reflLiteral :: Crude.Literal -> RefiningM F.Expr
reflLiteral (Crude.LiteralInteger n) = return $ F.expr n
-- !WARN proper way to create something of sort `realSort`?
reflLiteral (Crude.LiteralFloat x) = return $ F.ECon (F.R x)
reflLiteral (Crude.LiteralBit b) = return $ F.prop b
-- !WARN proper way to create something of sort `charSort`?
reflLiteral (Crude.LiteralChar c) = return $ F.expr (Text.pack [c])
-- !WARN proper way to create something of sort `strSort`?
reflLiteral (Crude.LiteralString s) = return $ F.expr (Text.pack s)

reflType :: Type -> RefiningM F.Sort
reflType (TypeNumber Crude.TypeInt _) = return F.intSort
reflType (TypeNumber Crude.TypeUInt _) = return F.intSort
reflType (TypeNumber Crude.TypeFloat _) = return F.realSort
reflType TypeBit = return F.boolSort
reflType TypeChar = return F.charSort
reflType (TypeArray TypeChar) = return F.strSort
reflType (TypeArray _ty) =
  -- F.fAppTC array_ArrayFTycon <$> reflType `traverse` [ty]
  error "!TODO use LH's built-in arrays"
reflType (TypeTuple ty1 ty2) = F.fAppTC tuple_TupleFTycon <$> reflType `traverse` [ty1, ty2]
reflType (TypeOptional ty) = F.fAppTC optional_OptionalFTycon <$> reflType `traverse` [ty]
reflType (TypeNamed tyId) = do
  locSym <- FlexM.defaultLocated $ makeTypeIdSymbol tyId
  return $ F.FTC (F.symbolFTycon locSym)

-- Defines symbols:
-- - type contructor: F.symbol structId
-- - term constructor: F.symbol structId
-- - field accessor: F.symbol (structId, fieldId)
reflStructure :: Structure -> RefiningM F.DataDecl
reflStructure Structure {..} = do
  structureTypeLocatedSymbol <- FlexM.defaultLocated $ makeTypeIdSymbol structureId
  structureConstructorLocatedSymbol <- FlexM.defaultLocated $ makeStructureConstructorSymbol structureId
  fields <-
    forM structureFields $
      bimapM
        (FlexM.defaultLocated . makeStructureFieldAccessorSymbol . (structureId,))
        reflType

  return
    F.DDecl
      { ddTyCon = F.symbolFTycon structureTypeLocatedSymbol,
        ddVars = 0,
        ddCtors =
          [ F.DCtor
              { dcName = structureConstructorLocatedSymbol,
                dcFields =
                  fields <&> \(fieldLocatedSymbol, fieldSort) ->
                    F.DField
                      { dfName = fieldLocatedSymbol,
                        dfSort = fieldSort
                      }
              }
          ]
      }

-- | Defines symbols:
-- - type constructor: F.symbol varntId
-- - term constructors: F.symbol (varntId, ctorId)
-- - field accessors: F.symbol (varntId, ctorId, fieldIx)
reflVariant :: Variant -> RefiningM F.DataDecl
reflVariant Variant {..} = do
  variantTypeLocatedSymbol <- FlexM.defaultLocated $ makeTypeIdSymbol variantId
  ctors <-
    forM variantConstructors $
      \(ctorId, fieldTypes) -> do
        ctor <- FlexM.defaultLocated (makeVariantConstructorSymbol (variantId, ctorId))
        fields <-
          (fieldTypes `zip` [0 :: Int ..]) <&*> \(fieldType, i) -> do
            fieldSymbol <- FlexM.defaultLocated $ makeVariantFieldAccessorSymbol (variantId, ctorId, i)
            fieldSort <- reflType fieldType
            return (fieldSymbol, fieldSort)

        return (ctor, fields)
  return
    F.DDecl
      { ddTyCon = F.symbolFTycon variantTypeLocatedSymbol,
        ddVars = 0,
        ddCtors =
          ctors <&> \(ctorLocatedSymbol, ctorParamTypes) ->
            F.DCtor
              { dcName = ctorLocatedSymbol,
                dcFields =
                  ctorParamTypes <&> \(ctorParamLocatedSymbol, ctorParamSort) ->
                    F.DField
                      { dfName = ctorParamLocatedSymbol,
                        dfSort = ctorParamSort
                      }
              }
      }
