module Language.Flex.Refining.Syntax where

import qualified Language.Flex.Syntax as Crude

-- * Refined Syntax

-- ** Types

data Type
  = TypeNumber Crude.NumberType Integer
  | TypeBit
  | TypeChar
  | TypeArray Type
  | TypeTuple [Type]
  | TypeOptional Type
  | TypeNamed Crude.TypeId
  deriving (Eq, Show)

-- ** Terms

data Term
  = TermLiteral {termLiteral :: Crude.Literal, termType :: Type}
  | TermPrimitive {termPrimitive :: Primitive, termType :: Type}
  | TermLet {termPattern :: Pattern, termTerm :: Term, termBody :: Term, termType :: Type}
  | TermAssert {termTerm :: Term, termBody :: Term, termType :: Type}
  | TermMember {termTerm :: Term, termFieldId :: Crude.FieldId, termType :: Type}
  | TermNamed {termId :: Crude.TermId, termType :: Type}
  | TermApplication {termFunctionId :: Crude.TermId, termArguments :: [Term], termType :: Type}
  | TermConstructor {termVariantId :: Crude.TypeId, termConstructorId :: Crude.TermId, termArguments :: [Term], termType :: Type}
  | TermStructure {termStructureId :: Crude.TypeId, termFields :: [Term], termType :: Type}
  | TermMatch {termTerm :: Term, termBranches :: [(Pattern, Term)], termType :: Type}
  deriving (Eq, Show)

data Pattern
  = PatternNamed Crude.TermId Type
  | PatternDiscard Type
  deriving (Eq, Show)

data Primitive
  = PrimitiveTry Term
  | PrimitiveTuple [Term]
  | PrimitiveArray [Term]
  | PrimitiveIf Term Term Term
  | PrimitiveAnd Term Term
  | PrimitiveOr Term Term
  | PrimitiveNot Term
  | PrimitiveEq Term Term
  | PrimitiveAdd Term Term
  | PrimitiveExtends Term Crude.TypeId
  deriving (Eq, Show)
