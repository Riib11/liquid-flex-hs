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
  = TermLiteral Literal ann
  | TermPrimitive (Primitive ann) ann
  | TermNamed TermId ann
  | TermBlock (Block ann) ann
  | TermStructure TypeId [(FieldId, Term ann)] ann
  | TermMember (Term ann) FieldId ann
  | TermApplication TermId [Term ann] (Maybe [Term ann]) ann
  | TermAscribe (Term ann) Type ann
  | TermMatch (Term ann) (Branches ann) ann
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