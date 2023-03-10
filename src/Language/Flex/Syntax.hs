module Language.Flex.Syntax where

import qualified Data.Map as Map
import qualified Language.Flex.Literal as Literal
import qualified Language.Flex.PrimitiveFunction as PrimitiveFunction

-- * Syntax

-- ** Idents

newtype ModuleId = ModuleId String
  deriving (Eq, Ord, Show)

newtype TypeId = TypeId String
  deriving (Eq, Ord, Show)

newtype TermId = TermId String
  deriving (Eq, Ord, Show)

newtype FunctionId = FunctionId String
  deriving (Eq, Ord, Show)

newtype ConstructorId = ConstructorId String
  deriving (Eq, Ord, Show)

newtype FieldId = FieldId String
  deriving (Eq, Ord, Show)

-- ** Module

data Module ann = Module
  { moduleId :: ModuleId,
    moduleDeclarations :: [Declaration ann]
  }

-- ** Declarations

data Declaration ann
  = DeclarationStructure (Structure ann)
  | DeclarationNewtype (Newtype ann)
  | DeclarationVariant (Variant ann)
  | DeclarationEnumerated (Enumerated ann)
  | DeclarationAlias (Alias ann)
  | DeclarationFunction (Function ann)
  | DeclarationConstant (Constant ann)
  deriving (Show)

data Structure ann = Structure
  { structureId :: TypeId,
    structureIsMessage :: Bool,
    structureFields :: Map.Map FieldId Type,
    structureRefinement :: Refinement ann,
    structureAnnotations :: [Annotation]
  }
  deriving (Show)

data Newtype ann = Newtype
  { newtypeId :: TypeId,
    newtypeIsMessage :: Bool,
    newtypeField :: (FieldId, Type),
    newtypeRefinement :: Refinement ann,
    newtypeAnnotations :: [Annotation]
  }
  deriving (Show)

data Variant ann = Variant
  { variantId :: TypeId,
    variantConstructors :: [(ConstructorId, [Type])],
    variantAnnotations :: [Annotation]
  }
  deriving (Show)

data Enumerated ann = Enumerated
  { enumeratedId :: TypeId,
    enumeratedType :: Type,
    enumeratedConstructors :: [(ConstructorId, Literal.Literal)]
  }
  deriving (Show)

data Alias ann = Alias
  { aliasId :: TypeId,
    aliasType :: Type
  }
  deriving (Show)

data Function ann = Function
  { functionId :: FunctionId,
    functionIsTransform :: Bool,
    functionParameters :: [(TermId, Type)],
    functionContextualParameters :: [(Type, TermId)],
    functionOutput :: Type,
    functionBody :: Term ann
  }
  deriving (Show)

data Constant ann = Constant
  { constantId :: TermId,
    constantTerm :: Term ann
  }
  deriving (Show)

-- ** Term

data Term ann
  = TermLiteral Literal.Literal ann
  | TermNamed TermId ann
  | TermBlock (Block ann)
  | TermStructure TypeId (Map.Map FieldId (Term ann)) ann
  | TermMember (Term ann) FieldId ann
  | TermConstructor ConstructorId (Maybe (Term ann)) ann
  | TermApplication Applicant [Term ann] (Maybe [Term ann]) ann
  | TermAscribe (Term ann) Type ann
  | TermMatch (Term ann) (Branches ann)
  deriving (Show, Functor)

data Applicant
  = ApplicantNamed FunctionId
  | ApplicantPrimitive PrimitiveFunction.PrimitiveFunction
  deriving (Show)

type Block ann = ([Statement ann], Term ann)

type Branches ann = [(Pattern, Term ann)]

-- ** Statement

data Statement ann
  = StatementLet Pattern (Term ann)
  | StatementAssert (Term ann)
  deriving (Show, Functor)

-- ** Pattern

data Pattern
  = PatternNamed TermId
  | PatternLiteral Literal.Literal
  | PatternDiscard
  deriving (Show)

-- ** Type

data Type
  = TypeNumber NumberType
  | TypeBit
  | TypeChar
  | TypeArray Type
  | TypeTuple [Type]
  | TypeOptional Type
  | TypeNamed TypeId
  | TypeUnifVar UnifVar
  | TypeStructure (Structure Type)
  | TypeEnumerated (Enumerated Type)
  | TypeVariant (Variant Type)
  | TypeNewtype (Newtype Type)
  deriving (Show)

data NumberType
  deriving (Show)

data UnifVar
  deriving (Show)

-- ** Refinement

newtype Refinement ann = Refinement (Term ann)
  deriving (Show)

-- ** Annotation

data Annotation deriving (Show)