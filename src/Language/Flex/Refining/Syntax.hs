module Language.Flex.Refining.Syntax where

import Data.Functor
import qualified Language.Fixpoint.Types as F
import Language.Flex.FlexM (MonadFlex)
import qualified Language.Flex.FlexM as FlexM
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
  deriving (Eq, Ord, Show)

instance Pretty Type where
  pPrint (TypeNumber nt n) = pPrint nt <> pPrint n
  pPrint TypeBit = "bit"
  pPrint TypeChar = "char"
  pPrint (TypeArray ty) = "Array" <> angles (pPrint ty)
  pPrint (TypeTuple ty1 ty2) = "Tuple" <> angles (commaList $ pPrint <$> [ty1, ty2])
  pPrint (TypeOptional ty) = "Optional" <> angles (pPrint ty)
  pPrint (TypeNamed tyId) = pPrint tyId

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
  | {- !TODO | TermPredicate {termSymbol :: !F.Symbol, termArguments :: ![Term], termType :: !Type} -}
    -- structures
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

trueTerm :: Term
trueTerm = TermLiteral (Crude.LiteralBit True) TypeBit

falseTerm :: Term
falseTerm = TermLiteral (Crude.LiteralBit False) TypeBit

letTerm :: Crude.TermId -> Term -> Term -> Term
letTerm tmId tm1 tm2 = TermLet (Just tmId) tm1 tm2 (termType tm2)

eqTerm :: Term -> Term -> Term
eqTerm tm1 tm2 = TermPrimitive (PrimitiveEq True tm1 tm2) TypeBit

neqTerm :: Term -> Term -> Term
neqTerm tm1 tm2 = TermPrimitive (PrimitiveEq False tm1 tm2) TypeBit

-- -- pow2Term n = 2^n
-- pow2Term :: Integer -> Term
-- pow2Term n = TermLiteral (Crude.LiteralInteger (2 ^ n)) (TypeNumber Crude.TypeInt 32)

-- -- pow2Term n = -(2^n)
-- negPow2Term :: Integer -> Int  Term
-- negPow2Term n size  = TermLiteral (Crude.LiteralInteger (-(2 ^ n))) (TypeNumber Crude.TypeInt size )

intTerm :: Integer -> Integer -> Term
intTerm n size = TermLiteral (Crude.LiteralInteger n) (TypeNumber Crude.TypeInt size)

floatTerm :: Double -> Integer -> Term
floatTerm x size = TermLiteral (Crude.LiteralFloat x) (TypeNumber Crude.TypeInt size)

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

-- | Includes partial casts are not resolved during typechecking, so need to
-- have them here (instantiated with the types they are casting between)
data Primitive
  = PrimitiveTry !Term
  | PrimitiveCast !Term !Type !Type
  | PrimitiveNone
  | PrimitiveSome !Term
  | PrimitiveTuple !Term !Term
  | PrimitiveFirst !Term
  | PrimitiveSecond !Term
  | PrimitiveArray ![Term]
  | PrimitiveIf !Term !Term !Term
  | PrimitiveNot !Term
  | PrimitiveEq Bool !Term !Term -- bool is False if negated
  | PrimitiveBoolBinOp Crude.BoolBinOp !Term !Term
  | PrimitiveNumBinOp Crude.NumBinOp !Term !Term
  | PrimitiveNumBinRel Crude.NumBinRel !Term !Term
  | PrimitiveExtends !Term !Crude.TypeId
  deriving (Eq, Show)

instance Pretty Primitive where
  pPrint (PrimitiveTry te) = parens $ "try" <+> pPrint te
  pPrint (PrimitiveCast tm ty1 ty2) = parens $ ("cast" <> braces (pPrint ty1 <+> "~>" <+> pPrint ty2)) <+> pPrint tm
  pPrint PrimitiveNone = "None"
  pPrint (PrimitiveSome tm) = "Some" <> parens (pPrint tm)
  pPrint (PrimitiveTuple te te') = parens $ commaList $ pPrint <$> [te, te']
  pPrint (PrimitiveFirst te) = parens $ "fst" <+> pPrint te
  pPrint (PrimitiveSecond te) = parens $ "snd" <+> pPrint te
  pPrint (PrimitiveArray tes) = brackets $ commaList $ pPrint <$> tes
  pPrint (PrimitiveIf te te' te2) = parens $ "if" <+> pPrint te <+> "then" <+> pPrint te' <+> pPrint te2
  pPrint (PrimitiveBoolBinOp bbo te te') = parens $ pPrint te <+> text (Crude.operatorOfBoolBinOp bbo) <+> pPrint te'
  pPrint (PrimitiveNot te) = parens $ "!" <+> pPrint te
  pPrint (PrimitiveEq True te te') = parens $ pPrint te <+> "==" <+> pPrint te'
  pPrint (PrimitiveEq False te te') = parens $ pPrint te <+> "!=" <+> pPrint te'
  pPrint (PrimitiveNumBinOp nbo te te') = parens $ pPrint te <+> text (Crude.operatorOfNumBinOp nbo) <+> pPrint te'
  pPrint (PrimitiveNumBinRel nbr te te') = parens $ pPrint te <+> text (Crude.operatorOfNumBinRel nbr) <+> pPrint te'
  pPrint (PrimitiveExtends te structId) = parens $ pPrint te <+> "extends" <+> pPrint structId

-- ** Converting to Symbols

makeTypeIdSymbol :: Crude.TypeId -> F.Symbol
makeTypeIdSymbol (Crude.TypeId x) = F.symbol x

makeDatatypePropertySymbol :: Crude.TypeId -> F.Symbol
makeDatatypePropertySymbol (Crude.TypeId x) = F.symbol ("property" <> x)

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
