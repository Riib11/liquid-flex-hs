module Language.Flex.Refining.Syntax where

import qualified Language.Fixpoint.Types as F
import qualified Language.Flex.Syntax as Crude
import Text.PrettyPrint.HughesPJClass hiding ((<>))

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

instance Pretty Function

-- ** Structure

data Structure = Structure
  { structureId :: Crude.TypeId,
    structureConstructorId :: Crude.TermId,
    structureFields :: [(Crude.FieldId, Type)],
    structureRefinement :: Crude.Refinement Type
  }
  deriving (Eq, Show)

type Fields = (Crude.FieldId, Type)

instance Pretty Structure

-- ** Variant

data Variant = Variant
  { variantId :: Crude.TypeId,
    variantConstructors :: [Constructor]
  }
  deriving (Eq, Show)

type Constructor = (Crude.TermId, [Type])

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
  | TermNamed {termId :: Crude.TermId, termType :: Type}
  | TermApplication {termFunctionId :: Crude.TermId, termArguments :: [Term], termType :: Type}
  | TermConstructor {termTypeId :: Crude.TypeId, termConstructorId :: Crude.TermId, termIsStructure :: Bool, termArguments :: [Term], termType :: Type}
  | TermMatch {termTerm :: Term, termBranches :: [(Pattern, Term)], termType :: Type}
  deriving (Eq, Show)

instance Pretty Term

data TermExpr = TermExpr {getTerm :: Term, getExpr :: F.Expr}

trueTerm :: Term
trueTerm = TermLiteral (Crude.LiteralBit True) TypeBit

falseTerm :: Term
falseTerm = TermLiteral (Crude.LiteralBit False) TypeBit

-- *** Pattern

data Pattern
  = PatternConstructor Crude.TypeId Crude.TermId [Crude.TermId]
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

eqTerm :: Term -> Term -> Term
eqTerm tm1 tm2 = TermPrimitive (PrimitiveEq tm1 tm2) TypeBit
