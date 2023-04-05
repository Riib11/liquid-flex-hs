module Language.Flex.Refining.Syntax where

import Data.Functor
import qualified Language.Fixpoint.Types as F
import qualified Language.Flex.Syntax as Crude
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility

-- * Refined Syntax

-- ** Function

data Function = Function
  { functionId :: Crude.TermId,
    functionIsTransform :: Bool,
    functionParameters :: [(Crude.TermId, Type)],
    functionOutput :: Type,
    functionBody :: Crude.Term Type
  }
  deriving (Eq, Show)

instance Pretty Function where
  pPrint Function {..} = (if functionIsTransform then "transform" else mempty) <+> "function" <+> pPrint functionId <+> parens (commaList $ functionParameters <&> \(tmId, ty) -> pPrint tmId <+> ":" <+> pPrint ty) <+> "->" <+> pPrint functionOutput <+> braces (pPrint functionBody)

-- ** Structure

data Structure = Structure
  { structureId :: Crude.TypeId,
    structureConstructorId :: Crude.TermId,
    structureFields :: [(Crude.FieldId, Type)],
    structureRefinement :: Crude.Refinement Type
  }
  deriving (Eq, Show)

type Fields = (Crude.FieldId, Type)

instance Pretty Structure where
  pPrint Structure {..} = "struct" <+> pPrint structureId <+> pPrint structureConstructorId <+> braces (semiList (structureFields <&> \(fieldId, ty) -> pPrint fieldId <+> ":" <+> pPrint ty) <+> semi <+> pPrint structureRefinement)

-- ** Variant

data Variant = Variant
  { variantId :: Crude.TypeId,
    variantConstructors :: [Constructor]
  }
  deriving (Eq, Show)

type Constructor = (Crude.TermId, [Type])

instance Pretty Variant where
  pPrint Variant {variantId, variantConstructors} = "variant" <+> pPrint variantId <+> braces (semiList $ variantConstructors <&> (\(ti, tys) -> pPrint ti <+> parens (commaList $ tys <&> pPrint)))

-- ** Type

data Type
  = TypeNumber !Crude.NumberType !Integer
  | TypeBit
  | TypeChar
  | TypeArray !Type
  | TypeTuple !Type !Type
  | TypeOptional !Type
  | TypeNamed !Crude.TypeId
  deriving (Eq, Show)

instance Pretty Type where
  pPrint (TypeNumber nt n) = pPrint nt <> pPrint n
  pPrint TypeBit = "bit"
  pPrint TypeChar = "char"
  pPrint (TypeArray ty) = "Array" <> angles (pPrint ty)
  pPrint (TypeTuple ty1 ty2) = "Tuple" <> angles (commaList $ pPrint <$> [ty1, ty2])
  pPrint (TypeOptional ty) = "Optional" <> angles (pPrint ty)
  pPrint (TypeNamed tyId) = pPrint tyId

data TypeSort = TypeSort {getType :: Type, getSort :: F.Sort}
  deriving (Show)

instance Pretty TypeSort where
  pPrint (TypeSort ty srt) = pPrint ty <+> parens ("reflected as:" <+> F.pprint srt)

-- ** Term

-- !TODO: separate structures and variants completely -- everywhere that i use
-- TerConstructor as if it could be a structure, change to only handle variants

data Term
  = TermLiteral {termLiteral :: !Crude.Literal, termType :: !Type}
  | TermPrimitive {termPrimitive :: !Primitive, termType :: !Type}
  | TermLet {termMaybeTermId :: !(Maybe Crude.TermId), termTerm :: !Term, termBody :: !Term, termType :: !Type}
  | TermAssert {termTerm :: !Term, termBody :: !Term, termType :: !Type}
  | TermNamed {termId :: !Crude.TermId, termType :: !Type}
  | TermApplication {termFunctionId :: !Crude.TermId, termArguments :: ![Term], termType :: !Type}
  | -- structures
    TermStructure {termStructId :: !Crude.TypeId, termFields :: ![Term], termType :: !Type}
  | TermMember {termStructId :: !Crude.TypeId, termTerm :: !Term, termFieldId :: !Crude.FieldId, termType :: !Type}
  | -- variants
    TermConstructor {termTypeId :: !Crude.TypeId, termConstructorId :: !Crude.TermId, termArguments :: ![Term], termType :: !Type}
  | TermMatch {termTerm :: !Term, termBranches :: ![Branch], termType :: !Type}
  deriving (Eq, Show)

type Branch = (Pattern, Term)

instance Pretty Term where
  pPrint (TermLiteral lit _ty) = pPrint lit
  pPrint (TermPrimitive prim _ty) = pPrint prim
  pPrint (TermLet mb_tmId tm1 tm2 _ty) = "let" <+> maybe "_" pPrint mb_tmId <+> "=" <+> pPrint tm1 <+> ";" <+> pPrint tm2
  pPrint (TermAssert tm1 tm2 _ty) = "assert" <+> pPrint tm1 <+> ";" <+> pPrint tm2
  pPrint (TermNamed tmId _ty) = pPrint tmId
  pPrint (TermApplication funId tms _ty) = pPrint funId <> parens (commaList $ pPrint <$> tms)
  pPrint (TermStructure structId fields _ty) = pPrint structId <> braces (commaList $ pPrint <$> fields)
  pPrint (TermMember _structId tm fieldId _ty) = pPrint tm <> "." <> pPrint fieldId
  pPrint (TermConstructor varntId ctorId tms _ty) = pPrint varntId <> "#" <> pPrint ctorId <> parens (commaList $ pPrint <$> tms)
  pPrint (TermMatch tm branches _ty) = "match" <+> pPrint tm <+> "with" <+> braces (semiList $ branches <&> \(pat, body) -> pPrint pat <+> "=>" <+> pPrint body)

pPrintShallowTerm :: Term -> Doc
pPrintShallowTerm (TermPrimitive prim _ty) = pPrintShallowPrimitive prim
pPrintShallowTerm (TermLet mb_tmId tm1 _tm2 _ty) = "let" <+> maybe "_" pPrint mb_tmId <+> "=" <+> pPrint tm1 <+> ";" <+> "..."
pPrintShallowTerm (TermAssert tm1 _tm2 _ty) = "assert" <+> pPrint tm1 <+> ";" <+> "..."
pPrintShallowTerm (TermMatch tm _branches _ty) = "match" <+> pPrint tm <+> "with" <+> braces "..."
pPrintShallowTerm term = pPrint term

pPrintShallowPrimitive :: Primitive -> Doc
pPrintShallowPrimitive (PrimitiveTry tm) = "try" <+> pPrintShallowTerm tm
pPrintShallowPrimitive (PrimitiveIf tm1 _tm2 _tm3) = "if" <+> pPrint tm1 <+> "then" <+> "..." <+> "else" <+> "..."
pPrintShallowPrimitive prim = pPrint prim

data TermExpr = TermExpr {getTerm :: !Term, getExpr :: !F.Expr}
  deriving (Show)

instance Pretty TermExpr where
  pPrint TermExpr {..} = pPrint getTerm <+> parens ("reflected as:" <+> F.pprint getExpr)

trueTerm :: Term
trueTerm = TermLiteral (Crude.LiteralBit True) TypeBit

falseTerm :: Term
falseTerm = TermLiteral (Crude.LiteralBit False) TypeBit

-- *** Pattern

data Pattern
  = PatternConstructor !Crude.TypeId !Crude.TermId ![Crude.TermId]
  | PatternNone
  | PatternSome !Crude.TermId
  deriving (Eq, Show)

instance Pretty Pattern where
  pPrint (PatternConstructor tyId ctorId tmIds) = pPrint tyId <> "#" <> pPrint ctorId <> parens (commaList $ pPrint <$> tmIds)
  pPrint PatternNone = "None"
  pPrint (PatternSome ti) = "Some" <> parens (pPrint ti)

-- *** Primitive

data Primitive
  = PrimitiveTry !Term
  | PrimitiveNone
  | PrimitiveSome !Term
  | PrimitiveTuple !Term !Term
  | PrimitiveArray ![Term]
  | PrimitiveIf !Term !Term !Term
  | PrimitiveAnd !Term !Term
  | PrimitiveOr !Term !Term
  | PrimitiveNot !Term
  | PrimitiveEq !Term !Term
  | PrimitiveAdd !Term !Term
  | PrimitiveExtends !Term !Crude.TypeId
  deriving (Eq, Show)

instance Pretty Primitive where
  pPrint (PrimitiveTry te) = parens $ "try" <+> pPrint te
  pPrint PrimitiveNone = "None"
  pPrint (PrimitiveSome tm) = "Some" <> parens (pPrint tm)
  pPrint (PrimitiveTuple te te') = parens $ commaList $ pPrint <$> [te, te']
  pPrint (PrimitiveArray tes) = brackets $ commaList $ pPrint <$> tes
  pPrint (PrimitiveIf te te' te2) = parens $ "if" <+> pPrint te <+> "then" <+> pPrint te' <+> pPrint te2
  pPrint (PrimitiveAnd te te') = parens $ pPrint te <+> "&&" <+> pPrint te'
  pPrint (PrimitiveOr te te') = parens $ pPrint te <+> "||" <+> pPrint te'
  pPrint (PrimitiveNot te) = parens $ "!" <+> pPrint te
  pPrint (PrimitiveEq te te') = parens $ pPrint te <+> "==" <+> pPrint te'
  pPrint (PrimitiveAdd te te') = parens $ pPrint te <+> "+" <+> pPrint te'
  pPrint (PrimitiveExtends te structId) = parens $ pPrint te <+> "extends" <+> pPrint structId

eqTerm :: Term -> Term -> Term
eqTerm tm1 tm2 = TermPrimitive (PrimitiveEq tm1 tm2) TypeBit

-- ** Converting to Symbols

makeTypeIdSymbol :: Crude.TypeId -> F.Symbol
makeTypeIdSymbol (Crude.TypeId x) = F.symbol x

makeStructurePropertySymbol :: Crude.TypeId -> F.Symbol
makeStructurePropertySymbol (Crude.TypeId x) = F.symbol ("property" <> x)

makeTermIdSymbol :: Crude.TermId -> F.Symbol
makeTermIdSymbol (Crude.TermId x) = F.symbol x

makeFieldIdSymbol :: Crude.FieldId -> F.Symbol
makeFieldIdSymbol (Crude.FieldId x) = F.symbol x

makeStructureConstructorSymbol :: Crude.TypeId -> F.Symbol
makeStructureConstructorSymbol (Crude.TypeId x) = F.symbol ("make" <> x)

makeStructureFieldAccessorSymbol :: (Crude.TypeId, Crude.FieldId) -> F.Symbol
makeStructureFieldAccessorSymbol (Crude.TypeId x, Crude.FieldId y) = F.symbol ("get" <> x <> y)

makeVariantConstructorSymbol :: (Crude.TypeId, Crude.TermId) -> F.Symbol
makeVariantConstructorSymbol (Crude.TypeId x, Crude.TermId y) = F.symbol ("make" <> x <> y)

makeVariantFieldAccessorSymbol :: (Crude.TypeId, Crude.TermId, Int) -> F.Symbol
makeVariantFieldAccessorSymbol (Crude.TypeId x, Crude.TermId y, i) = F.symbol ("get" <> x <> y <> show i)
