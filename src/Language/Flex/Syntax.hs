{-# HLINT ignore "Use newtype instead of data" #-}

module Language.Flex.Syntax where

import Data.Bifunctor (Bifunctor (bimap, second))
import Data.Functor ((<&>))
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Language.Fixpoint.Types as F
import Text.PrettyPrint.HughesPJClass (Doc, Pretty (pPrint), braces, brackets, colon, comma, doubleQuotes, equals, hcat, hsep, nest, parens, punctuate, quotes, semi, space, text, vcat, ($$), (<+>))
import Utility
import Prelude hiding (Enum)

-- * Syntax

data Syntax ann
  = SyntaxDeclaration (Declaration ann)
  | SyntaxType Type
  | SyntaxTerm (Term ann)

instance Pretty ann => Pretty (Syntax ann) where
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

-- *** F.Symbolic instances

instance F.Symbolic TypeId where
  symbol (TypeId x) = F.symbol x

-- | The term-level constructor for a value of a type. Relevant for Structures
-- and Newtypes in particular.
newtype TypeTermConstructor = TypeTermConstructor TypeId

instance F.Symbolic TypeTermConstructor where
  symbol (TypeTermConstructor (TypeId x)) = F.symbol $ "make$" <> x

instance F.Symbolic TermId where
  symbol (TermId x) = F.symbol x

instance F.Symbolic FieldId where
  symbol (FieldId y) = F.symbol y

instance F.Symbolic (TypeId, FieldId) where
  symbol (TypeId x, FieldId y) = F.symbol $ x <> "#" <> y

-- newtype FieldReference = FieldReference (TypeId, FieldId)

-- instance F.Symbolic FieldReference where
--   symbol (FieldReference (TypeId x, FieldId y)) = F.symbol $ x <> "#" <> y

-- newtype FieldProjector = FieldProjector (TypeId, FieldId)

-- instance F.Symbolic FieldProjector where
--   symbol (FieldProjector (TypeId x, FieldId y)) = F.symbol $ "proj$" <> x <> "#" <> y

fromFieldIdToTermId :: FieldId -> TermId
fromFieldIdToTermId (FieldId x) = TermId x

fromNewtypeIdToTermId :: TypeId -> TermId
fromNewtypeIdToTermId (TypeId x) = TermId x

-- ** Module

data Module ann = Module
  { moduleId :: ModuleId,
    moduleDeclarations :: [Declaration ann]
  }
  deriving (Eq, Functor, Foldable, Traversable, Show)

instance Pretty ann => Pretty (Module ann) where
  pPrint (Module {..}) =
    ("module" <+> pPrint moduleId <+> "where")
      $$ vcat (pPrint <$> moduleDeclarations)

-- ** Declarations

data Declaration ann
  = DeclarationStructure Structure
  | DeclarationNewtype (Newtype Type)
  | DeclarationVariant (Variant Type)
  | DeclarationEnum (Enum Type)
  | DeclarationAlias Alias
  | DeclarationFunction (Function ann)
  | DeclarationConstant (Constant ann)
  | DeclarationRefinedType (RefinedType ann)
  deriving (Eq, Functor, Foldable, Traversable, Show)

instance Pretty ann => Pretty (Declaration ann) where
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

instance ToDeclaration (Newtype Type) ann where
  toDeclaration = DeclarationNewtype

instance ToDeclaration (Variant Type) ann where
  toDeclaration = DeclarationVariant

instance ToDeclaration (Enum Type) ann where
  toDeclaration = DeclarationEnum

instance ToDeclaration Alias ann where
  toDeclaration = DeclarationAlias

instance ToDeclaration (Function ann) ann where
  toDeclaration = DeclarationFunction

instance ToDeclaration (Constant ann) ann where
  toDeclaration = DeclarationConstant

instance ToDeclaration (RefinedType ann) ann where
  toDeclaration = DeclarationRefinedType

pPrintDeclarationHeader :: Declaration ann -> Doc
pPrintDeclarationHeader =
  \case
    (DeclarationStructure Structure {..}) -> "structure" <+> pPrint structureId
    (DeclarationNewtype Newtype {..}) -> "newtype" <+> pPrint newtypeId
    (DeclarationVariant Variant {..}) -> "variant" <+> pPrint variantId
    (DeclarationEnum Enum {..}) -> "enum" <+> pPrint enumId
    (DeclarationAlias Alias {..}) -> "alias" <+> pPrint aliasId
    (DeclarationFunction Function {..}) -> "function" <+> pPrint functionId
    (DeclarationConstant Constant {..}) -> "constant" <+> pPrint constantId
    (DeclarationRefinedType RefinedType {..}) -> "refine" <+> pPrint refinedTypeId

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
               (pPrint fieldId <+> colon <+> pPrint ty) <> semi
         )
      $$ "}"

-- RefinedType
data RefinedType ann = RefinedType
  { refinedTypeId :: TypeId,
    refinedTypeRefinement :: Refinement ann
  }
  deriving (Eq, Functor, Foldable, Traversable, Show)

instance Pretty ann => Pretty (RefinedType ann) where
  pPrint (RefinedType {..}) =
    "#refine" <> angles (pPrint refinedTypeId) <> parens (pPrint refinedTypeRefinement)

-- *** Newtype

data Newtype ann = Newtype
  { newtypeId :: TypeId,
    newtypeConstructorId :: TermId,
    newtypeFieldId :: FieldId,
    newtypeType :: ann
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Pretty ann => Pretty (Newtype ann) where
  pPrint (Newtype {..}) = pPrint newtypeId <+> equals <+> pPrint newtypeType

-- *** Variant

data Variant ann = Variant
  { variantId :: TypeId,
    variantConstructors :: [(TermId, [ann])]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Pretty ann => Pretty (Variant ann) where
  pPrint (Variant {..}) =
    vcat
      [ pPrint variantId <+> "{",
        nest 2 . vcat $
          variantConstructors <&> \(tmId, tys) ->
            pPrint tmId <> (hcat . punctuate (comma <> space) $ pPrint <$> tys),
        "}"
      ]

-- *** Enum

data Enum ann = Enum
  { enumId :: TypeId,
    enumType :: ann,
    enumConstructors :: [(TermId, Literal)]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Pretty ann => Pretty (Enum ann) where
  pPrint (Enum {..}) =
    vcat
      [ pPrint enumId <+> pPrint enumType <+> "{",
        nest 2 . vcat $
          enumConstructors <&> \(tmId, lit) ->
            pPrint tmId <+> equals <+> pPrint lit,
        "}"
      ]

-- *** Alias

data Alias = Alias
  { aliasId :: TypeId,
    aliasType :: Type
  }
  deriving (Eq, Show)

instance Pretty Alias where
  pPrint (Alias {..}) = pPrint aliasId <+> equals <+> pPrint aliasType

-- *** Function

data Function ann = Function
  { functionId :: TermId,
    functionIsTransform :: Bool,
    functionParameters :: [(TermId, Type)],
    -- | since contextual parameters must have newtypes, data Term only need to store the
    -- newtypes' ids
    functionContextualParameters :: Maybe [(TypeId, TermId)],
    functionOutput :: Type,
    functionBody :: Term ann
  }
  deriving (Eq, Functor, Foldable, Traversable, Show)

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
      tuple = parens . hcat . punctuate (comma <> space)

-- *** Constant

data Constant ann = Constant
  { constantId :: TermId,
    constantBody :: Term ann,
    constantType :: Type
  }
  deriving (Eq, Functor, Foldable, Traversable, Show)

instance Pretty (Constant ann) where
  pPrint (Constant {..}) =
    "constant"
      <+> pPrint constantId
      <+> equals
      <+> pPrint constantBody

-- ** Term

-- Each constructor has a `termAnn :: ann`
data Term ann
  = TermLiteral {termLiteral :: Literal, termAnn :: ann}
  | TermPrimitive {termPrimitive :: Primitive ann, termAnn :: ann}
  | TermLet {termPattern :: Pattern ann, termTerm :: Term ann, termBody :: Term ann, termAnn :: ann}
  | TermAssert {termTerm :: Term ann, termBody :: Term ann, termAnn :: ann}
  | TermStructure {termStructureId :: TypeId, termFields :: [(FieldId, Term ann)], termAnn :: ann}
  | TermMember {termTerm :: Term ann, termFieldId :: FieldId, termAnn :: ann}
  | TermNeutral {termApplicant :: Applicant ann, termMaybeArgs :: Maybe [Term ann], termMaybeCxargs :: Maybe [Term ann], termAnn :: ann}
  | TermAscribe {termTerm :: Term ann, termType :: Type, termAnn :: ann}
  | TermMatch {termTerm :: Term ann, termBranches :: Branches ann, termAnn :: ann}
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Applicant ann = Applicant
  { applicantMaybeTypeId :: Maybe TypeId,
    applicantTermId :: TermId,
    applicantAnn :: ApplicantType ann
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

termIdApplicant :: TermId -> ApplicantType ann -> Applicant ann
termIdApplicant applicantTermId applicantAnn =
  Applicant
    { applicantMaybeTypeId = Nothing,
      applicantTermId,
      applicantAnn
    }

instance Eq ann => Ord (Applicant ann) where
  compare app1 app2 = case compare (applicantMaybeTypeId app1) (applicantMaybeTypeId app2) of
    LT -> LT
    GT -> GT
    EQ -> compare (applicantTermId app1) (applicantTermId app2)

instance Pretty (Applicant ann) where
  pPrint (Applicant {..}) = case applicantMaybeTypeId of
    Nothing -> pPrint applicantTermId
    Just tyId -> pPrint tyId <> "." <> pPrint applicantTermId

type Branches ann = [(Pattern ann, Term ann)]

instance Pretty (Term ann) where
  pPrint = \case
    TermLiteral {termLiteral} -> pPrint termLiteral
    TermPrimitive {termPrimitive} -> pPrint termPrimitive
    TermLet {termPattern, termTerm, termBody} ->
      text "let" <+> pPrint termPattern <+> equals <+> pPrint termTerm <+> semi <+> pPrint termBody
    TermAssert {termTerm, termBody} ->
      text "assert" <+> pPrint termTerm <+> semi <+> pPrint termBody
    TermStructure {termStructureId, termFields} ->
      pPrint termStructureId
        <> "{"
        <> ( hcat . punctuate (semi <> space) $
               termFields <&> \(tmId, tm) ->
                 pPrint tmId <+> equals <+> pPrint tm
           )
        <> "}"
    TermMember {termTerm, termFieldId} ->
      pPrint termTerm <> "." <> pPrint termFieldId
    TermNeutral {termApplicant = Applicant Nothing termId _, termMaybeArgs, termMaybeCxargs} ->
      ( pPrint termId
          <> ( case termMaybeArgs of
                 Nothing -> mempty
                 Just args -> parens . hcat . punctuate (comma <> space) $ pPrint <$> args
             )
      )
        <+> ( case termMaybeCxargs of
                Nothing -> mempty
                Just cxargs -> "giving" <+> (parens . hcat . punctuate (comma <> space) $ pPrint <$> cxargs)
            )
    TermNeutral {termApplicant = Applicant (Just typeId) termId _, termMaybeArgs, termMaybeCxargs} ->
      ( (pPrint typeId <> "#" <> pPrint termId)
          <> ( case termMaybeArgs of
                 Nothing -> mempty
                 Just args -> parens . hcat . punctuate (comma <> space) $ pPrint <$> args
             )
      )
        <+> ( case termMaybeCxargs of
                Nothing -> mempty
                Just cxargs -> "giving" <+> (parens . hcat . punctuate (comma <> space) $ pPrint <$> cxargs)
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
    PrimitiveTuple tms -> parens . hcat . punctuate (comma <> space) . fmap pPrint $ tms
    PrimitiveArray tms -> braces . hcat . punctuate (comma <> space) . fmap pPrint $ tms
    PrimitiveIf tm1 tm2 tm3 -> parens $ hsep ["if", pPrint tm1, "then", pPrint tm2, "else", pPrint tm3]
    PrimitiveAnd tm1 tm2 -> parens $ hsep [pPrint tm1, "&&", pPrint tm2]
    PrimitiveOr tm1 tm2 -> parens $ hsep [pPrint tm1, "||", pPrint tm2]
    PrimitiveNot tm -> "!" <> pPrint tm
    PrimitiveEq tm1 tm2 -> parens $ pPrint tm1 <+> "==" <+> pPrint tm2
    PrimitiveAdd tm1 tm2 -> parens $ pPrint tm1 <+> "+" <+> pPrint tm2

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
  | TypeEnum (Enum Type)
  | TypeVariant (Variant Type)
  | TypeNewtype (Newtype Type)
  deriving (Eq, Show)

data ApplicantType ann
  = ApplicantTypeFunction (FunctionType ann)
  | ApplicantTypeEnumConstructor (Enum ann) TermId
  | ApplicantTypeVariantConstructor (Variant ann) TermId [ann]
  | ApplicantTypeNewtypeConstructor (Newtype ann)
  | ApplicantType ann
  deriving (Eq, Show, Functor, Foldable, Traversable)

data FunctionType ann = FunctionType
  { functionTypeId :: TermId,
    functionTypeIsTransform :: Bool,
    functionTypeParameters :: [(TermId, ann)],
    functionTypeContextualParameters :: Maybe [(TypeId, TermId)],
    functionTypeOutput :: ann
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

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
      tuple = parens . hcat . punctuate (comma <> space)

instance Pretty ann => Pretty (FunctionType ann) where
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
      tuple = parens . hcat . punctuate (comma <> space)

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

newtype Refinement ann = Refinement {unRefinement :: Term ann}
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
  pPrint (Refinement tm) = "assert" <> parens (pPrint tm)

renameTerm :: Map.Map TermId TermId -> Term r -> Term r
renameTerm tmIds term = case term of
  TermLiteral {} -> term
  TermPrimitive prim r -> TermPrimitive (renamePrimitive tmIds prim) r
  -- there's no shadowing, so nothing special to worry about here
  TermLet pat te te' r -> TermLet pat (renameTerm tmIds te) (renameTerm tmIds te') r
  TermAssert te te' r -> TermAssert (renameTerm tmIds te) (renameTerm tmIds te') r
  TermStructure ti fields r -> TermStructure ti (second (renameTerm tmIds) <$> fields) r
  TermMember te fi r -> TermMember (renameTerm tmIds te) fi r
  TermNeutral ap m_tes m_te's r -> TermNeutral ap (renameTerm tmIds <$$> m_tes) (renameTerm tmIds <$$> m_te's) r
  TermAscribe te ty r -> TermAscribe (renameTerm tmIds te) ty r
  TermMatch te branches r -> TermMatch (renameTerm tmIds te) (second (renameTerm tmIds) <$> branches) r

renamePrimitive :: Map.Map TermId TermId -> Primitive r -> Primitive r
renamePrimitive tmIds prim = case prim of
  PrimitiveTry te -> PrimitiveTry (renameTerm tmIds te)
  PrimitiveCast te -> PrimitiveCast (renameTerm tmIds te)
  PrimitiveTuple tes -> PrimitiveTuple (renameTerm tmIds <$> tes)
  PrimitiveArray tes -> PrimitiveArray (renameTerm tmIds <$> tes)
  PrimitiveIf te te' te_r -> PrimitiveIf (renameTerm tmIds te) (renameTerm tmIds te') (renameTerm tmIds te_r)
  PrimitiveAnd te te' -> PrimitiveAnd (renameTerm tmIds te) (renameTerm tmIds te')
  PrimitiveOr te te' -> PrimitiveOr (renameTerm tmIds te) (renameTerm tmIds te')
  PrimitiveNot te -> PrimitiveNot (renameTerm tmIds te)
  PrimitiveEq te te' -> PrimitiveEq (renameTerm tmIds te) (renameTerm tmIds te')
  PrimitiveAdd te te' -> PrimitiveAdd (renameTerm tmIds te) (renameTerm tmIds te')

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
