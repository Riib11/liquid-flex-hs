{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Language.Flex.Syntax where

import Data.Bifunctor (Bifunctor (bimap, second))
import Data.List (intercalate)
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJClass (Doc, Pretty (pPrint), braces, brackets, colon, comma, doubleQuotes, hsep, nest, parens, punctuate, quotes, text, vcat, ($$), (<+>))
import Utility
import Prelude hiding (Enum)

-- * Syntax

data Syntax ann
  = SyntaxDeclaration (Declaration ann)
  | SyntaxType Type
  | SyntaxTerm (Term ann)

instance Pretty (Syntax ann) where
  pPrint = \case
    SyntaxDeclaration de -> pPrint de
    SyntaxType ty -> pPrint ty
    SyntaxTerm te -> pPrint te

class ToSyntax a ann where
  toSyntax :: a -> Syntax ann

instance ToSyntax (Declaration ann) ann where
  toSyntax = SyntaxDeclaration

instance ToSyntax Type ann where
  toSyntax = SyntaxType

instance ToSyntax (Term ann) ann where
  toSyntax = SyntaxTerm

-- ** Idents

newtype ModuleId = ModuleId String
  deriving (Eq, Ord, Show)

instance Pretty ModuleId where
  pPrint (ModuleId x) = text x

-- TODO: although I originally tried to separate these, they actually kinda all
-- live in the same namespace, so I should probably just merge them

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

fromFieldIdToTermId :: FieldId -> TermId
fromFieldIdToTermId (FieldId x) = TermId x

fromNewtypeIdToTermId :: TypeId -> TermId
fromNewtypeIdToTermId (TypeId x) = TermId x

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
  = DeclarationStructure Structure
  | DeclarationNewtype Newtype
  | DeclarationVariant Variant
  | DeclarationEnum Enum
  | DeclarationAlias Alias
  | DeclarationFunction (Function ann)
  | DeclarationConstant (Constant ann)
  | DeclarationRefinedType (RefinedType ann)
  deriving (Eq, Show)

instance Pretty (Declaration ann) where
  pPrint = \case
    DeclarationStructure struct -> pPrint struct
    DeclarationNewtype newty -> pPrint newty
    DeclarationVariant varnt -> pPrint varnt
    DeclarationEnum enum -> pPrint enum
    DeclarationAlias alias -> pPrint alias
    DeclarationFunction fun -> pPrint fun
    DeclarationConstant con -> pPrint con
    DeclarationRefinedType refnStruct -> pPrint refnStruct

class ToDeclaration a ann where
  toDeclaration :: a -> Declaration ann

instance ToDeclaration Structure ann where
  toDeclaration = DeclarationStructure

instance ToDeclaration Newtype ann where
  toDeclaration = DeclarationNewtype

instance ToDeclaration Variant ann where
  toDeclaration = DeclarationVariant

instance ToDeclaration Enum ann where
  toDeclaration = DeclarationEnum

instance ToDeclaration Alias ann where
  toDeclaration = DeclarationAlias

instance ToDeclaration (Function ann) ann where
  toDeclaration = DeclarationFunction

instance ToDeclaration (Constant ann) ann where
  toDeclaration = DeclarationConstant

instance ToDeclaration (RefinedType ann) ann where
  toDeclaration = DeclarationRefinedType

-- *** Structure

data Structure = Structure
  { structureId :: TypeId,
    structureIsMessage :: Bool,
    structureMaybeExtensionId :: Maybe TypeId,
    structureFields :: [(FieldId, Type)]
  }
  deriving (Eq, Show)

instance Pretty Structure where
  pPrint (Structure {..}) =
    hsep
      [ if structureIsMessage then "message" else mempty,
        "structure",
        pPrint structureId,
        case structureMaybeExtensionId of
          Nothing -> mempty
          Just extId -> "extends" <+> pPrint extId,
        "{"
      ]
      $$ ( nest 2 . vcat $
             structureFields <&> \(fieldId, ty) ->
               (pPrint fieldId <+> colon <+> pPrint ty) <> ";"
         )
      $$ "}"

-- RefinedType
data RefinedType ann = RefinedType
  { refinedTypeId :: TypeId,
    refinedTypeRefinement :: Refinement ann
  }
  deriving (Eq, Show)

instance Pretty (RefinedType ann) where
  pPrint (RefinedType {..}) =
    "#refine" <> angles (pPrint refinedTypeId) <> parens (pPrint refinedTypeRefinement)

-- *** Newtype

data Newtype = Newtype
  { newtypeId :: TypeId,
    newtypeConstructorId :: TermId,
    newtypeFieldId :: FieldId,
    newtypeType :: Type
  }
  deriving (Eq, Show)

instance Pretty Newtype where
  pPrint (Newtype {..}) = pPrint newtypeId <+> "=" <+> pPrint newtypeType

-- *** Variant

data Variant = Variant
  { variantId :: TypeId,
    variantConstructors :: [(TermId, [Type])]
  }
  deriving (Eq, Show)

instance Pretty Variant where
  pPrint (Variant {..}) =
    vcat
      [ pPrint variantId <+> "{",
        nest 2 . vcat $
          variantConstructors <&> \(tmId, tys) ->
            pPrint tmId <> (hsep . punctuate comma $ pPrint <$> tys),
        "}"
      ]

-- *** Enum

data Enum = Enum
  { enumId :: TypeId,
    enumType :: Type,
    enumConstructors :: [(TermId, Literal)]
  }
  deriving (Eq, Show)

instance Pretty Enum where
  pPrint (Enum {..}) =
    vcat
      [ pPrint enumId <+> pPrint enumType <+> "{",
        nest 2 . vcat $
          enumConstructors <&> \(tmId, lit) ->
            pPrint tmId <+> "=" <+> pPrint lit,
        "}"
      ]

-- *** Alias

data Alias = Alias
  { aliasId :: TypeId,
    aliasType :: Type
  }
  deriving (Eq, Show)

instance Pretty Alias where
  pPrint (Alias {..}) = pPrint aliasId <+> "=" <+> pPrint aliasType

-- *** Function

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
  deriving (Eq, Show)

instance Pretty (Function ann) where
  pPrint (Function {..}) =
    vcat
      [ (pPrint functionId <> parameters functionParameters)
          <+> ( case functionContextualParameters of
                  Nothing -> mempty
                  Just cxparams -> "giving" <+> parameters cxparams
              )
          <+> "->"
          <+> pPrint functionOutput
          <+> text "{",
        nest 2 $ pPrint functionBody,
        text "}"
      ]
    where
      parameters :: (Pretty a, Pretty b) => [(a, b)] -> Doc
      parameters params = tuple $ params <&> \(a, b) -> pPrint a <+> colon <+> pPrint b

      tuple :: [Doc] -> Doc
      tuple = parens . hsep . punctuate comma

-- *** Constant

data Constant ann = Constant
  { constantId :: TermId,
    constantTerm :: Term ann,
    constantType :: Type
  }
  deriving (Eq, Show)

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
  | TermNeutral {termApplicant :: Applicant, termMaybeArgs :: Maybe [Term ann], termMaybeCxargs :: Maybe [Term ann], termAnn :: ann}
  | TermAscribe {termTerm :: Term ann, termType :: Type, termAnn :: ann}
  | TermMatch {termTerm :: Term ann, termBranches :: Branches ann, termAnn :: ann}
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Applicant = Applicant (Maybe TypeId, TermId)
  deriving (Eq, Ord, Show)

instance Pretty Applicant where
  pPrint (Applicant (Just tyId, tmId)) = pPrint tyId <> "." <> pPrint tmId
  pPrint (Applicant (Nothing, tmId)) = pPrint tmId

type Block ann = ([Statement ann], Term ann)

type Branches ann = [(Pattern ann, Term ann)]

instance Pretty (Term ann) where
  pPrint = \case
    TermLiteral {termLiteral} -> pPrint termLiteral
    TermPrimitive {termPrimitive} -> pPrint termPrimitive
    TermBlock {termBlock = (stmts, tm)} -> "{" $$ nest 2 (vcat $ (pPrint <$> stmts) ++ [pPrint tm]) $$ "}"
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
    TermNeutral {termApplicant = Applicant (Nothing, termId), termMaybeArgs, termMaybeCxargs} ->
      ( pPrint termId
          <> ( case termMaybeArgs of
                 Nothing -> mempty
                 Just args -> parens . hsep . punctuate comma $ pPrint <$> args
             )
      )
        <+> ( case termMaybeCxargs of
                Nothing -> mempty
                Just cxargs -> "giving" <+> (parens . hsep . punctuate comma $ pPrint <$> cxargs)
            )
    TermNeutral {termApplicant = Applicant (Just typeId, termId), termMaybeArgs, termMaybeCxargs} ->
      ( (pPrint typeId <> "#" <> pPrint termId)
          <> ( case termMaybeArgs of
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
  | PrimitiveEq (Term ann) (Term ann)
  | PrimitiveAdd (Term ann) (Term ann)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Pretty (Primitive ann) where
  pPrint = \case
    PrimitiveTry tm -> "try" <> parens (pPrint tm)
    PrimitiveCast tm -> "cast" <> parens (pPrint tm)
    PrimitiveTuple tms -> parens . hsep . punctuate comma . fmap pPrint $ tms
    PrimitiveArray tms -> braces . hsep . punctuate comma . fmap pPrint $ tms
    PrimitiveIf tm1 tm2 tm3 -> parens $ hsep ["if", pPrint tm1, "then", pPrint tm2, "else", pPrint tm3]
    PrimitiveAnd tm1 tm2 -> parens $ hsep [pPrint tm1, "&&", pPrint tm2]
    PrimitiveOr tm1 tm2 -> parens $ hsep [pPrint tm1, "||", pPrint tm2]
    PrimitiveNot tm -> "!" <> pPrint tm
    PrimitiveEq tm1 tm2 -> parens $ pPrint tm1 <+> "==" <+> pPrint tm2
    PrimitiveAdd tm1 tm2 -> parens $ pPrint tm1 <+> "+" <+> pPrint tm2

-- ** Statement

data Statement ann
  = StatementLet (Pattern ann) (Term ann)
  | StatementAssert (Term ann)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Pretty (Statement ann) where
  pPrint = \case
    StatementLet pat tm -> ("let" <+> pPrint pat <+> "=" <+> pPrint tm) <> ";"
    StatementAssert tm -> "assert" <> parens (pPrint tm) <> ";"

-- ** Pattern

data Pattern ann
  = PatternNamed TermId ann
  | PatternDiscard ann
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Pretty (Pattern ann) where
  pPrint = \case
    PatternNamed tmId _ -> pPrint tmId
    PatternDiscard _ -> "_"

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
  | TypeStructure Structure
  | TypeEnum Enum
  | TypeVariant Variant
  | TypeNewtype Newtype
  deriving (Eq, Show)

data FunctionType = FunctionType
  { functionTypeId :: TermId,
    functionTypeIsTransform :: Bool,
    functionTypeParameters :: [(TermId, Type)],
    functionTypeContextualParameters :: Maybe [(TypeId, TermId)],
    functionTypeOutput :: Type
  }
  deriving (Eq, Show)

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
    TypeStructure struct -> pPrint (structureId struct)
    TypeEnum enum -> pPrint (enumId enum)
    TypeVariant varnt -> pPrint (variantId varnt)
    TypeNewtype newty -> pPrint (newtypeId newty)
    where
      tuple :: [Doc] -> Doc
      tuple = parens . hsep . punctuate comma

instance Pretty FunctionType where
  pPrint FunctionType {..} =
    hsep
      [ pPrint functionTypeId,
        if functionTypeIsTransform then "transform" else mempty,
        parameters functionTypeParameters,
        case functionTypeContextualParameters of
          Nothing -> mempty
          Just cxparams -> "given" <+> parameters cxparams
      ]
    where
      parameters :: (Pretty a, Pretty b) => [(a, b)] -> Doc
      parameters params = tuple $ params <&> \(a, b) -> pPrint a <+> colon <+> pPrint b

      tuple :: [Doc] -> Doc
      tuple = parens . hsep . punctuate comma

data NumberType
  = TypeInt
  | TypeUInt
  | TypeFloat
  deriving (Eq, Show)

instance Pretty NumberType where
  pPrint = \case
    TypeInt -> "int"
    TypeUInt -> "uint"
    TypeFloat -> "float"

data UnifyVar = UnifyVar String Int
  deriving (Eq, Ord, Show)

instance Pretty UnifyVar where
  pPrint (UnifyVar str i) = "?" <> brackets (text str) <> text "#" <> pPrint i

data UnifyConstraint
  = UnifyConstraintCasted Type
  | UnifyConstraintNumeric
  deriving (Eq, Show)

instance Pretty UnifyConstraint where
  pPrint = \case
    UnifyConstraintCasted ty -> "UnifyConstraintCasted" <> parens (pPrint ty)
    UnifyConstraintNumeric -> "UnifyConstraintNumeric"

-- ** Literal

data Literal
  = LiteralInteger Integer
  | LiteralFloat Double
  | LiteralBit Bool
  | LiteralChar Char
  | LiteralString String
  deriving (Eq, Show)

instance Pretty Literal where
  pPrint = \case
    LiteralInteger n -> pPrint n
    LiteralFloat x -> pPrint x
    LiteralBit b -> text if b then "true" else "false"
    LiteralChar c -> quotes $ pPrint c
    LiteralString s -> doubleQuotes $ text s

-- ** Refinement

newtype Refinement ann = Refinement (Term ann)
  deriving (Eq, Show, Functor, Traversable, Foldable)

trueRefinement :: Refinement ()
trueRefinement = Refinement (TermLiteral (LiteralBit True) ())

andRefinement :: Refinement () -> Refinement () -> Refinement ()
andRefinement (Refinement tm1) (Refinement tm2) = Refinement (TermPrimitive (PrimitiveAnd tm1 tm2) ())

andRefinements :: [Refinement ()] -> Refinement ()
andRefinements [] = trueRefinement
andRefinements [rfn] = rfn
andRefinements (rfn : rfns) = foldr andRefinement rfn rfns

instance Pretty (Refinement ann) where
  pPrint tm = "assert" <> parens (pPrint tm)

-- -- substTerm x a b = b[x := a]
-- substTerm :: TermId -> Term r -> Term r -> Term r
-- substTerm tmId tm' = go
--   where
--     go term = case term of
--       TermLiteral _lit _r -> term
--       TermPrimitive _prim _r -> term
--       TermBlock block r -> TermBlock (bimap (goStatement <$>) go block) r
--       TermStructure ti fields r -> TermStructure ti (second go <$> fields) r
--       TermMember te fi r -> TermMember (go te) fi r
--       TermNeutral (Applicant (Nothing, tmId')) Nothing Nothing _r | tmId == tmId' -> tm'
--       TermNeutral ap m_tes m_te's r -> TermNeutral ap (go <$$> m_tes) (go <$$> m_te's) r
--       TermAscribe te ty r -> TermAscribe (go te) ty r
--       TermMatch te branches r -> TermMatch (go te) (second go <$> branches) r
--     goStatement = \case
--       StatementLet pat te -> StatementLet pat (go te)
--       StatementAssert te -> StatementAssert (go te)
