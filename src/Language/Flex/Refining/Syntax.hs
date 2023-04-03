module Language.Flex.Refining.Syntax where

import qualified Language.Flex.Syntax as Crude
import Text.PrettyPrint.HughesPJClass hiding ((<>))

-- * Refined Syntax

-- ** Function

data Function = Function
  { functionId :: Crude.TermId,
    functionIsTranform :: Bool,
    functionParameters :: [(Crude.TermId, Type)],
    functionBody :: Term
  }
  deriving (Eq, Show)

instance Pretty Function

-- ** Structure

data Structure = Structure
  { structureId :: Crude.TypeId,
    structureConstructorId :: Crude.TermId,
    structureFields :: [(Crude.FieldId, Type)],
    structureRefinement :: Term
  }
  deriving (Eq, Show)

instance Pretty Structure

-- ** Variant

data Variant = Variant
  { variantId :: Crude.TypeId,
    variantConstructors :: [(Crude.TermId, [Type])]
  }
  deriving (Eq, Show)

instance Pretty Variant

-- ** Type

data Type
  = TypeNumber Crude.NumberType Integer
  | TypeBit
  | TypeChar
  | TypeArray Type
  | TypeTuple Type Type
  | TypeOptional Type
  | TypeNamed Crude.TypeId
  deriving (Eq, Show)

instance Pretty Type

-- ** Term

data Term
  = TermLiteral {termLiteral :: Crude.Literal, termType :: Type}
  | TermPrimitive {termPrimitive :: Primitive, termType :: Type}
  | TermLet {termMaybeTermId :: Maybe Crude.TermId, termTerm :: Term, termBody :: Term, termType :: Type}
  | TermAssert {termTerm :: Term, termBody :: Term, termType :: Type}
  | TermMember {termTerm :: Term, termFieldId :: Crude.FieldId, termType :: Type}
  | TermNamed {termId :: Crude.TermId, termType :: Type}
  | TermApplication {termFunctionId :: Crude.TermId, termArguments :: [Term], termType :: Type}
  | TermConstructor {termVariantId :: Crude.TypeId, termConstructorId :: Crude.TermId, termArguments :: [Term], termType :: Type}
  | TermStructure {termStructureId :: Crude.TypeId, termFields :: [Term], termType :: Type}
  | TermMatch {termTerm :: Term, termBranches :: [(Pattern, Term)], termType :: Type}
  deriving (Eq, Show)

instance Pretty Term

-- *** Pattern

data Pattern
  = PatternNamed Crude.TermId Type
  | PatternDiscard Type
  deriving (Eq, Show)

instance Pretty Pattern

-- *** Primitive

data Primitive
  = PrimitiveTry Term
  | PrimitiveTuple Term Term
  | PrimitiveArray [Term]
  | PrimitiveIf Term Term Term
  | PrimitiveAnd Term Term
  | PrimitiveOr Term Term
  | PrimitiveNot Term
  | PrimitiveEq Term Term
  | PrimitiveAdd Term Term
  | PrimitiveExtends Term Crude.TypeId
  deriving (Eq, Show)

instance Pretty Primitive