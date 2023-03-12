module Language.Flex.Syntax where

import qualified Data.Map as Map

-- * Syntax

-- ** Idents

newtype ModuleId = ModuleId String
  deriving (Eq, Ord, Show)

newtype TypeId = TypeId String
  deriving (Eq, Ord, Show)

newtype TermId = TermId String
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
    newtypeField :: (FieldId, Type),
    newtypeRefinement :: Refinement ann,
    newtypeAnnotations :: [Annotation]
  }
  deriving (Show)

data Variant ann = Variant
  { variantId :: TypeId,
    variantConstructors :: [(TermId, [Type])],
    variantAnnotations :: [Annotation]
  }
  deriving (Show)

data Enumerated ann = Enumerated
  { enumeratedId :: TypeId,
    enumeratedType :: Type,
    enumeratedConstructors :: [(TermId, Literal)]
  }
  deriving (Show)

data Alias ann = Alias
  { aliasId :: TypeId,
    aliasType :: Type
  }
  deriving (Show)

data Function ann = Function
  { functionId :: TermId,
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
  = TermLiteral {termLiteral :: Literal, termAnn :: ann}
  | TermPrimitive {termPrimitive :: Primitive ann, termAnn :: ann}
  | TermNamed {termId :: TermId, termAnn :: ann}
  | TermBlock {termBlock :: Block ann, termAnn :: ann}
  | TermStructure {termStructureId :: TypeId, termFields :: [(FieldId, Term ann)], termAnn :: ann}
  | TermMember {termTerm :: Term ann, termFieldId :: FieldId, termAnn :: ann}
  | TermApplication {termId :: TermId, termArgs :: [Term ann], termMaybeCxargs :: Maybe [Term ann], termAnn :: ann}
  | TermAscribe {termTerm :: Term ann, termType :: Type, ternAnn :: ann}
  | TermMatch {termTerm :: Term ann, termBranches :: Branches ann, termAnn :: ann}
  deriving (Show, Functor, Foldable, Traversable)

type Block ann = ([Statement ann], Term ann)

type Branches ann = [(Pattern, Term ann)]

-- ** Primitive

data Primitive ann
  = PrimitiveTry (Term ann)
  | PrimitiveCast (Term ann)
  | PrimitiveTuple [Term ann]
  | PrimitiveArray [Term ann]
  | PrimitiveIf (Term ann) (Term ann) (Term ann)
  | PrimitiveAnd (Term ann) (Term ann)
  | PrimitiveOr (Term ann) (Term ann)
  | PrimitiveNot (Term ann)
  deriving (Show, Functor, Foldable, Traversable)

-- ** Statement

data Statement ann
  = StatementLet Pattern (Term ann)
  | StatementAssert (Term ann)
  deriving (Show, Functor, Foldable, Traversable)

-- ** Pattern

data Pattern
  = PatternNamed TermId
  | PatternLiteral Literal
  | PatternDiscard
  deriving (Show)

-- ** Type

data Type
  = TypeNumber NumberType Integer
  | TypeBit
  | TypeChar
  | TypeArray Type
  | TypeTuple [Type]
  | TypeOptional Type
  | TypeNamed TypeId
  | TypeUnifyVar UnifyVar (Maybe UnifyConstraint)
  | TypeStructure (Structure Type)
  | TypeEnumerated (Enumerated Type)
  | TypeVariant (Variant Type)
  | TypeNewtype (Newtype Type)
  deriving (Show)

data NumberType
  = TypeInt
  | TypeUInt
  | TypeFloat
  deriving (Show)

newtype UnifyVar = UnifyVar Int
  deriving (Eq, Ord, Show)

data UnifyConstraint
  = CastedFrom Type
  deriving (Show)

-- ** Literal

data Literal
  = LiteralInteger Integer
  | LiteralFloat Float
  | LiteralBool Bool
  | LiteralChar Char
  | LiteralString String
  deriving (Show)

-- ** Refinement

newtype Refinement ann = Refinement (Term ann)
  deriving (Show)

-- ** Annotation

data Annotation deriving (Show)