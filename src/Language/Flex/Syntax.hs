{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Flex.Syntax where

import Data.List (intercalate)
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJClass (Doc, Pretty (pPrint), braces, brackets, colon, comma, doubleQuotes, hsep, nest, parens, punctuate, quotes, text, vcat, ($$), (<+>))
import Utility

-- * Syntax

-- ** Idents

newtype ModuleId = ModuleId String
  deriving (Eq, Ord, Show)

instance Pretty ModuleId where
  pPrint (ModuleId x) = text x

newtype TypeId = TypeId String
  deriving (Eq, Ord, Show)

instance Pretty TypeId where
  pPrint (TypeId x) = text x

newtype TermId = TermId String
  deriving (Eq, Ord, Show)

instance Pretty TermId where
  pPrint (TermId x) = text x

newtype FieldId = FieldId String
  deriving (Eq, Ord, Show)

instance Pretty FieldId where
  pPrint (FieldId x) = text x

-- ** Module

data Module ann = Module
  { moduleId :: ModuleId,
    moduleDeclarations :: [Declaration ann]
  }

instance Pretty (Module ann) where
  pPrint (Module {..}) =
    ("module" <+> pPrint moduleId)
      $$ vcat (pPrint <$> moduleDeclarations)

-- ** Declarations

data Declaration ann
  = DeclarationStructure (Structure ann)
  | DeclarationNewtype (Newtype ann)
  | DeclarationVariant (Variant ann)
  | DeclarationEnumerated (Enumerated ann)
  | DeclarationAlias (Alias ann)
  | DeclarationFunction (Function ann)
  | DeclarationConstant (Constant ann)
  | DeclarationRefinedStructure (RefinedStructure ann)
  deriving (Show)

instance Pretty (Declaration ann) where
  pPrint = \case
    DeclarationStructure struct -> pPrint struct
    DeclarationNewtype newty -> pPrint newty
    DeclarationVariant varnt -> pPrint varnt
    DeclarationEnumerated enume -> pPrint enume
    DeclarationAlias alias -> pPrint alias
    DeclarationFunction fun -> pPrint fun
    DeclarationConstant con -> pPrint con
    DeclarationRefinedStructure reftStruct -> pPrint reftStruct

data Structure ann = Structure
  { structureId :: TypeId,
    structureIsMessage :: Bool,
    structureExtensionid :: Maybe TypeId,
    structureFields :: [(FieldId, Type)]
  }
  deriving (Show)

instance Pretty (Structure ann) where
  pPrint (Structure {..}) =
    hsep
      [ if structureIsMessage then "message" else mempty,
        "structure",
        pPrint structureId,
        case structureExtensionid of
          Nothing -> mempty
          Just extId -> "extends" <+> pPrint extId,
        "{"
      ]
      $$ ( nest 4 . vcat $
             structureFields <&> \(fieldId, ty) ->
               (pPrint fieldId <+> colon <+> pPrint ty) <> ";"
         )
      $$ "}"

data RefinedStructure ann = RefinedStructure
  { refinedStructureId :: TypeId,
    refinedStructureRefinement :: Refinement ann
  }
  deriving (Show)

instance Pretty (RefinedStructure ann) where
  pPrint (RefinedStructure {..}) =
    "#" <> "refine" <> brackets (pPrint refinedStructureId) <> parens (pPrint refinedStructureRefinement)

data Newtype ann = Newtype
  { newtypeId :: TypeId,
    newtypeType :: Type
  }
  deriving (Show)

instance Pretty (Newtype ann) where
  pPrint (Newtype {..}) = pPrint newtypeId <+> "=" <+> pPrint newtypeType

data Variant ann = Variant
  { variantId :: TypeId,
    variantConstructors :: [(TermId, Maybe [Type])]
  }
  deriving (Show)

instance Pretty (Variant ann) where
  pPrint (Variant {..}) =
    vcat
      [ pPrint variantId <+> "{",
        nest 4 . vcat $
          variantConstructors <&> \(tmId, mb_tys) ->
            pPrint tmId
              <> case mb_tys of
                Nothing -> mempty
                Just tys -> hsep . punctuate comma $ pPrint <$> tys,
        "}"
      ]

data Enumerated ann = Enumerated
  { enumeratedId :: TypeId,
    enumeratedType :: Type,
    enumeratedConstructors :: [(TermId, Literal)]
  }
  deriving (Show)

instance Pretty (Enumerated ann) where
  pPrint (Enumerated {..}) =
    vcat
      [ pPrint enumeratedId <+> pPrint enumeratedType <+> "{",
        nest 4 . vcat $
          enumeratedConstructors <&> \(tmId, lit) ->
            pPrint tmId <+> "=" <+> pPrint lit,
        "}"
      ]

data Alias ann = Alias
  { aliasId :: TypeId,
    aliasType :: Type
  }
  deriving (Show)

instance Pretty (Alias ann) where
  pPrint (Alias {..}) = pPrint aliasId <+> "=" <+> pPrint aliasType

data Function ann = Function
  { functionId :: TermId,
    functionIsTransform :: Bool,
    functionParameters :: [(TermId, Type)],
    -- | since contextual parameters must have newtypes, only need to store the
    -- newtypes' ids
    functionContextualParameters :: Maybe [(TypeId, TermId)],
    functionOutput :: Type,
    functionBody :: Term ann
  }
  deriving (Show)

instance Pretty (Function ann) where
  pPrint (Function {..}) =
    vcat
      [ (pPrint functionId <> parameters functionParameters)
          <+> ( case functionContextualParameters of
                  Nothing -> mempty
                  Just cxparams -> "giving" <+> parameters cxparams
              )
          <+> "->"
          <+> pPrint functionOutput,
        nest 4 $ pPrint functionBody
      ]
    where
      parameters :: (Pretty a, Pretty b) => [(a, b)] -> Doc
      parameters params = tuple $ params <&> \(a, b) -> pPrint a <+> colon <+> pPrint b

      tuple :: [Doc] -> Doc
      tuple = parens . hsep . punctuate comma

data Constant ann = Constant
  { constantId :: TermId,
    constantTerm :: Term ann
  }
  deriving (Show)

instance Pretty (Constant ann) where
  pPrint (Constant {..}) =
    "constant"
      <+> pPrint constantId
      <+> "="
      <+> pPrint constantTerm

-- ** Term

-- Each constructor has a `termAnn :: ann`
data Term ann
  = TermLiteral {termLiteral :: Literal, termAnn :: ann}
  | TermPrimitive {termPrimitive :: Primitive ann, termAnn :: ann}
  | TermBlock {termBlock :: Block ann, termAnn :: ann}
  | TermStructure {termStructureId :: TypeId, termFields :: [(FieldId, Term ann)], termAnn :: ann}
  | TermMember {termTerm :: Term ann, termFieldId :: FieldId, termAnn :: ann}
  | TermNeutral {termId :: TermId, termArgs :: Maybe [Term ann], termMaybeCxargs :: Maybe [Term ann], termAnn :: ann}
  | TermAscribe {termTerm :: Term ann, termType :: Type, termAnn :: ann}
  | TermMatch {termTerm :: Term ann, termBranches :: Branches ann, termAnn :: ann}
  deriving (Show, Functor, Foldable, Traversable)

type Block ann = ([Statement ann], Term ann)

type Branches ann = [(Pattern, Term ann)]

instance Pretty (Term ann) where
  pPrint = \case
    TermLiteral {termLiteral} -> pPrint termLiteral
    TermPrimitive {termPrimitive} -> pPrint termPrimitive
    TermBlock {termBlock = (stmts, tm)} -> "{" $$ nest 4 (vcat $ (pPrint <$> stmts) ++ [pPrint tm]) $$ "}"
    TermStructure {termStructureId, termFields} ->
      pPrint termStructureId
        <> "{"
        <> ( hsep . punctuate ";" $
               termFields <&> \(tmId, tm) ->
                 pPrint tmId <+> "=" <+> pPrint tm
           )
        <> "}"
    TermMember {termTerm, termFieldId} ->
      pPrint termTerm <> "." <> pPrint termFieldId
    TermNeutral {termId, termArgs, termMaybeCxargs} ->
      ( pPrint termId
          <> ( case termArgs of
                 Nothing -> mempty
                 Just args -> parens . hsep . punctuate comma $ pPrint <$> args
             )
      )
        <+> ( case termMaybeCxargs of
                Nothing -> mempty
                Just cxargs -> "giving" <+> (parens . hsep . punctuate comma $ pPrint <$> cxargs)
            )
    TermAscribe {termTerm, termType} ->
      pPrint termTerm <+> colon <+> pPrint termType
    TermMatch {termTerm, termBranches} ->
      "match"
        <+> pPrint termTerm
        <+> "with"
        <+> vcat
          ( termBranches <&> \(pat, tm) ->
              "|" <+> pPrint pat <+> "=>" <+> pPrint tm
          )

-- only maps over the top `termAnn`
mapTopAnnTerm :: (ann -> ann) -> Term ann -> Term ann
mapTopAnnTerm f term = term {termAnn = f $ termAnn term}

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

instance Pretty (Primitive ann) where
  pPrint = \case
    PrimitiveTry tm -> "try" <> parens (pPrint tm) <> ")"
    PrimitiveCast tm -> "cast" <> parens (pPrint tm) <> ")"
    PrimitiveTuple tms -> parens . hsep . punctuate comma . fmap pPrint $ tms
    PrimitiveArray tms -> braces . hsep . punctuate comma . fmap pPrint $ tms
    PrimitiveIf tm1 tm2 tm3 -> parens $ hsep ["if", pPrint tm1, "then", pPrint tm2, "else", pPrint tm3]
    PrimitiveAnd tm1 tm2 -> parens $ hsep [pPrint tm1, "&&", pPrint tm2]
    PrimitiveOr tm1 tm2 -> parens $ hsep [pPrint tm1, "||", pPrint tm2]
    PrimitiveNot tm -> "!" <> pPrint tm

-- ** Statement

data Statement ann
  = StatementLet Pattern (Term ann)
  | StatementAssert (Term ann)
  deriving (Show, Functor, Foldable, Traversable)

instance Pretty (Statement ann) where
  pPrint = \case
    StatementLet pat tm -> ("let" <+> pPrint pat <+> "=" <+> pPrint tm) <> ";"
    StatementAssert tm -> "assert" <> parens (pPrint tm) <> ";"

-- ** Pattern

data Pattern
  = PatternNamed TermId
  | PatternLiteral Literal
  | PatternDiscard
  deriving (Show)

instance Pretty Pattern where
  pPrint = \case
    PatternNamed tmId -> pPrint tmId
    PatternLiteral lit -> pPrint lit
    PatternDiscard -> "_"

-- ** Type

data Type
  = TypeNumber NumberType Integer
  | TypeBit
  | TypeChar
  | TypeArray Type
  | TypeTuple [Type]
  | TypeOptional Type
  | TypeNamed TypeId
  | -- the types below cannot be written directly by the user; they are only
    -- introduced during typing
    TypeUnifyVar UnifyVar (Maybe UnifyConstraint)
  | TypeFunction (Function Type)
  | TypeStructure (Structure Type)
  | TypeEnumerated (Enumerated Type)
  | TypeVariant (Variant Type)
  | TypeNewtype (Newtype Type)
  | TypeVariantConstuctor (Variant Type) TermId (Maybe [Type])
  | TypeEnumConstructor (Enumerated Type) TermId
  | TypeNewtypeConstructor (Newtype Type)
  deriving (Show)

instance Pretty Type where
  pPrint = \case
    TypeNumber nt n -> pPrint nt <> pPrint n
    TypeBit -> "bit"
    TypeChar -> "char"
    TypeArray ty -> "Array<" <> pPrint ty <> ">"
    TypeTuple tys -> "Tuple<" <> (tuple . fmap pPrint $ tys) <> ">"
    TypeOptional ty -> "Optional<" <> pPrint ty <> ">"
    TypeNamed ti -> pPrint ti
    TypeUnifyVar uv mb_uc -> case mb_uc of
      Nothing -> pPrint uv
      Just uc -> pPrint uv <> "{" <> pPrint uc <> "}"
    TypeFunction func ->
      hsep
        [ pPrint (functionId func),
          if functionIsTransform func then "transform" else mempty,
          parameters $ functionParameters func,
          case functionContextualParameters func of
            Nothing -> mempty
            Just cxparams -> "given" <+> parameters cxparams
        ]
    TypeStructure struct -> pPrint (structureId struct)
    TypeEnumerated enume -> pPrint (enumeratedId enume)
    TypeVariant varnt -> pPrint (variantId varnt)
    TypeNewtype newty -> pPrint (newtypeId newty)
    TypeVariantConstuctor varnt constrId mb_tyParams ->
      (pPrint (variantId varnt) <> "." <> pPrint constrId)
        <> case mb_tyParams of
          Nothing -> mempty
          Just tys -> tuple $ pPrint <$> tys
    TypeEnumConstructor enume constrId ->
      pPrint (enumeratedId enume) <> "." <> pPrint constrId
    TypeNewtypeConstructor newty ->
      pPrint (newtypeId newty)
    where
      parameters :: (Pretty a, Pretty b) => [(a, b)] -> Doc
      parameters params = tuple $ params <&> \(a, b) -> pPrint a <+> colon <+> pPrint b

      tuple :: [Doc] -> Doc
      tuple = parens . hsep . punctuate comma

data NumberType
  = TypeInt
  | TypeUInt
  | TypeFloat
  deriving (Show)

instance Pretty NumberType where
  pPrint = \case
    TypeInt -> "int"
    TypeUInt -> "uint"
    TypeFloat -> "float"

newtype UnifyVar = UnifyVar Int
  deriving (Eq, Ord, Show)

instance Pretty UnifyVar where
  pPrint (UnifyVar i) = "?" <> pPrint i

data UnifyConstraint
  = CastedFrom Type
  deriving (Show)

instance Pretty UnifyConstraint where
  pPrint = \case
    CastedFrom ty -> "CastedFrom" <> parens (pPrint ty)

-- ** Literal

data Literal
  = LiteralInteger Integer
  | LiteralFloat Float
  | LiteralBool Bool
  | LiteralChar Char
  | LiteralString String
  deriving (Show)

instance Pretty Literal where
  pPrint = \case
    LiteralInteger n -> pPrint n
    LiteralFloat x -> pPrint x
    LiteralBool b -> pPrint b
    LiteralChar c -> quotes $ pPrint c
    LiteralString s -> doubleQuotes $ pPrint s

-- ** Refinement

newtype Refinement ann = Refinement (Term ann)
  deriving (Show)

instance Pretty (Refinement ann) where
  pPrint tm = "assert" <> parens (pPrint tm)
