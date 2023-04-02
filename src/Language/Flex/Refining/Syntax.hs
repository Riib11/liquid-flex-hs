module Language.Flex.Refining.Syntax where

import qualified Language.Flex.Syntax as Crude
import Text.PrettyPrint.HughesPJClass hiding ((<>))

-- * Refined Syntax

-- ** Types

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

-- ** Terms

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

data Pattern
  = PatternNamed Crude.TermId Type
  | PatternDiscard Type
  deriving (Eq, Show)

instance Pretty Pattern

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