{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Language.Flex.Syntax where

import Data.Bifunctor (Bifunctor (bimap, second))
import Data.Functor ((<&>))
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Language.Fixpoint.Types as F
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility
import Prelude hiding (Enum)

-- * Syntax

data Syntax ty tm
  = SyntaxDeclaration (Declaration ty tm)
  | SyntaxType Type
  | SyntaxTerm (Term tm)
  | SyntaxPattern (Pattern tm)

instance (Pretty ty, Pretty tm) => Pretty (Syntax ty tm) where
  pPrint = \case
    SyntaxDeclaration de -> pPrint de
    SyntaxType ty -> pPrint ty
    SyntaxTerm te -> pPrint te
    SyntaxPattern pat -> pPrint pat

class ToSyntax a ty tm where
  toSyntax :: a -> Syntax ty tm

instance ToSyntax (Declaration ty tm) ty tm where
  toSyntax = SyntaxDeclaration

instance ToSyntax Type ty tm where
  toSyntax = SyntaxType

instance ToSyntax (Term tm) ty tm where
  toSyntax = SyntaxTerm

instance ToSyntax (Pattern tm) ty tm where
  toSyntax = SyntaxPattern

-- ** Idents

newtype ModuleId = ModuleId String
  deriving (Eq, Ord, Show)

instance Pretty ModuleId where
  pPrint (ModuleId x) = text x

-- !TODO although I originally tried to separate these, they actually kinda all
-- live in the same namespace, so I should probably just merge them

newtype TypeId = TypeId String
  deriving (Eq, Ord, Show)

tupleTypeId :: TypeId
tupleTypeId = TypeId "Tuple"

optionalTypeId :: TypeId
optionalTypeId = TypeId "Optional"

arrayTypeId :: TypeId
arrayTypeId = TypeId "Array"

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

fromStructureIdToTermId :: TypeId -> TermId
fromStructureIdToTermId (TypeId x) = TermId x

-- ** Ty, Tm

-- Newtype wrappers for deriving Functor, Foldable, and Traversable instances
-- over the Ty and Tm annoations respectively

newtype Ty f tm ty = Ty {unTy :: f ty tm}

newtype Tm f ty tm = Tm {unTm :: f ty tm}

fmapTy f x = unTy $ fmap f (Ty x)

fmapTm f x = unTm $ fmap f (Tm x)

traverseTy f x = unTy <$> f `traverse` Ty x

traverseTm f x = unTm <$> f `traverse` Tm x

-- ** Module

-- @ty@ is the type of types, and @tm@ is the type of type annotations on terms
data Module ty tm = Module
  { moduleId :: ModuleId,
    moduleDeclarations :: [Declaration ty tm]
  }
  deriving (Eq, Show)

instance (Pretty tm, Pretty ty) => Pretty (Module ty tm) where
  pPrint (Module {..}) =
    ("module" <+> pPrint moduleId <+> "where")
      $$ vcat (pPrint <$> moduleDeclarations)

instance Functor (Ty Module tm) where
  fmap f (Ty mdl@Module {..}) = Ty mdl {moduleDeclarations = unTy . fmap f . Ty <$> moduleDeclarations}

instance Foldable (Ty Module tm) where
  foldMap f (Ty Module {..}) = foldMap (foldMap f . Ty) moduleDeclarations

instance Traversable (Ty Module tm) where
  traverse f (Ty Module {..}) =
    Ty . (\decls -> Module {moduleId, moduleDeclarations = decls})
      <$> ((f `traverseTy`) `traverse` moduleDeclarations)

instance Functor (Tm Module ty) where
  fmap f (Tm mdl@Module {..}) = Tm mdl {moduleDeclarations = unTm . fmap f . Tm <$> moduleDeclarations}

instance Foldable (Tm Module ty) where
  foldMap f (Tm Module {..}) = foldMap (foldMap f . Tm) moduleDeclarations

instance Traversable (Tm Module ty) where
  traverse f (Tm Module {..}) =
    Tm . (\decls -> Module {moduleId, moduleDeclarations = decls})
      <$> ((f `traverseTm`) `traverse` moduleDeclarations)

-- ** Declarations

data Declaration ty tm
  = DeclarationStructure (Structure ty)
  | DeclarationNewtype (Newtype ty)
  | DeclarationVariant (Variant ty)
  | DeclarationEnum (Enum ty)
  | DeclarationAlias (Alias ty)
  | DeclarationFunction (Function ty tm)
  | DeclarationConstant (Constant ty tm)
  | DeclarationRefinedType (RefinedType tm)
  deriving (Eq, Show)

instance (Pretty ty, Pretty tm) => Pretty (Declaration ty tm) where
  pPrint = \case
    DeclarationStructure struct -> pPrint struct
    DeclarationNewtype newty -> pPrint newty
    DeclarationVariant varnt -> pPrint varnt
    DeclarationEnum enum -> pPrint enum
    DeclarationAlias alias -> pPrint alias
    DeclarationFunction fun -> pPrint fun
    DeclarationConstant con -> pPrint con
    DeclarationRefinedType refnStruct -> pPrint refnStruct

instance Functor (Ty Declaration tm) where
  fmap f (Ty (DeclarationStructure struc)) = Ty (DeclarationStructure (f <$> struc))
  fmap f (Ty (DeclarationNewtype new)) = Ty (DeclarationNewtype (f <$> new))
  fmap f (Ty (DeclarationVariant vari)) = Ty (DeclarationVariant (f <$> vari))
  fmap f (Ty (DeclarationEnum en)) = Ty (DeclarationEnum (f <$> en))
  fmap f (Ty (DeclarationAlias al)) = Ty (DeclarationAlias (f <$> al))
  fmap f (Ty (DeclarationFunction func)) = Ty (DeclarationFunction (f `fmapTy` func))
  fmap f (Ty (DeclarationConstant con)) = Ty (DeclarationConstant (f `fmapTy` con))
  fmap _f (Ty (DeclarationRefinedType rt)) = Ty (DeclarationRefinedType rt)

instance Foldable (Ty Declaration tm) where
  foldMap f (Ty (DeclarationStructure struc)) = foldMap f struc
  foldMap f (Ty (DeclarationNewtype new)) = foldMap f new
  foldMap f (Ty (DeclarationVariant vari)) = foldMap f vari
  foldMap f (Ty (DeclarationEnum en)) = foldMap f en
  foldMap f (Ty (DeclarationAlias al)) = foldMap f al
  foldMap f (Ty (DeclarationFunction func)) = foldMap f (Ty func)
  foldMap f (Ty (DeclarationConstant con)) = foldMap f (Ty con)
  foldMap _f (Ty (DeclarationRefinedType _rt)) = mempty

instance Traversable (Ty Declaration tm) where
  traverse f (Ty (DeclarationStructure struc)) = Ty . toDeclaration <$> traverse f struc
  traverse f (Ty (DeclarationNewtype new)) = Ty . toDeclaration <$> traverse f new
  traverse f (Ty (DeclarationVariant vari)) = Ty . toDeclaration <$> traverse f vari
  traverse f (Ty (DeclarationEnum en)) = Ty . toDeclaration <$> traverse f en
  traverse f (Ty (DeclarationAlias al)) = Ty . toDeclaration <$> traverse f al
  traverse f (Ty (DeclarationFunction func)) = Ty . toDeclaration <$> traverseTy f func
  traverse f (Ty (DeclarationConstant con)) = Ty . toDeclaration <$> traverseTy f con
  traverse _f (Ty (DeclarationRefinedType rt)) = pure (Ty (DeclarationRefinedType rt))

instance Functor (Tm Declaration ty) where
  fmap _f (Tm (DeclarationStructure struc)) = Tm (DeclarationStructure struc)
  fmap _f (Tm (DeclarationNewtype new)) = Tm (DeclarationNewtype new)
  fmap _f (Tm (DeclarationVariant vari)) = Tm (DeclarationVariant vari)
  fmap _f (Tm (DeclarationEnum en)) = Tm (DeclarationEnum en)
  fmap _f (Tm (DeclarationAlias al)) = Tm (DeclarationAlias al)
  fmap f (Tm (DeclarationFunction func)) = Tm (DeclarationFunction (f `fmapTm` func))
  fmap f (Tm (DeclarationConstant con)) = Tm (DeclarationConstant (f `fmapTm` con))
  fmap f (Tm (DeclarationRefinedType rt)) = Tm (DeclarationRefinedType (f <$> rt))

instance Foldable (Tm Declaration tm) where
  foldMap _f (Tm (DeclarationStructure _struc)) = mempty
  foldMap _f (Tm (DeclarationNewtype _new)) = mempty
  foldMap _f (Tm (DeclarationVariant _vari)) = mempty
  foldMap _f (Tm (DeclarationEnum _en)) = mempty
  foldMap _f (Tm (DeclarationAlias _al)) = mempty
  foldMap f (Tm (DeclarationFunction func)) = foldMap f (Tm func)
  foldMap f (Tm (DeclarationConstant con)) = foldMap f (Tm con)
  foldMap f (Tm (DeclarationRefinedType rt)) = foldMap f rt

instance Traversable (Tm Declaration ty) where
  traverse _f (Tm (DeclarationStructure struc)) = pure (Tm (DeclarationStructure struc))
  traverse _f (Tm (DeclarationNewtype new)) = pure (Tm (DeclarationNewtype new))
  traverse _f (Tm (DeclarationVariant vari)) = pure (Tm (DeclarationVariant vari))
  traverse _f (Tm (DeclarationEnum en)) = pure (Tm (DeclarationEnum en))
  traverse _f (Tm (DeclarationAlias al)) = pure (Tm (DeclarationAlias al))
  traverse f (Tm (DeclarationFunction func)) = Tm . toDeclaration <$> traverseTm f func
  traverse f (Tm (DeclarationConstant con)) = Tm . toDeclaration <$> traverseTm f con
  traverse f (Tm (DeclarationRefinedType rt)) = Tm . toDeclaration <$> traverse f rt

class ToDeclaration a ty tm where
  toDeclaration :: a -> Declaration ty tm

instance ToDeclaration (Structure ty) ty tm where
  toDeclaration = DeclarationStructure

instance ToDeclaration (Newtype ty) ty tm where
  toDeclaration = DeclarationNewtype

instance ToDeclaration (Variant ty) ty tm where
  toDeclaration = DeclarationVariant

instance ToDeclaration (Enum ty) ty tm where
  toDeclaration = DeclarationEnum

instance ToDeclaration (Alias ty) ty tm where
  toDeclaration = DeclarationAlias

instance ToDeclaration (Function ty tm) ty tm where
  toDeclaration = DeclarationFunction

instance ToDeclaration (Constant ty tm) ty tm where
  toDeclaration = DeclarationConstant

instance ToDeclaration (RefinedType tm) ty tm where
  toDeclaration = DeclarationRefinedType

pPrintDeclarationHeader :: Declaration ty tm -> Doc
pPrintDeclarationHeader =
  \case
    (DeclarationStructure Structure {..}) -> "structure" <+> pPrint structureId
    (DeclarationNewtype Newtype {..}) -> "newtype" <+> pPrint newtypeId
    (DeclarationVariant Variant {..}) -> "variant" <+> pPrint variantId
    (DeclarationEnum Enum {..}) -> "enum" <+> pPrint enumId
    (DeclarationAlias Alias {..}) -> "alias" <+> pPrint aliasId
    (DeclarationFunction Function {..}) -> "function" <+> pPrint functionId
      where
        FunctionType {..} = functionType
    (DeclarationConstant Constant {..}) -> "constant" <+> pPrint constantId
    (DeclarationRefinedType RefinedType {..}) -> "refine" <+> pPrint refinedTypeId

-- *** Structure

-- !TODO why isn't this parametrized by a type variable in place of @Type@?
data Structure ann = Structure
  { structureId :: TypeId,
    structureIsMessage :: Bool,
    structureMaybeExtensionId :: Maybe TypeId,
    structureFields :: [(FieldId, ann)]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Pretty ann => Pretty (Structure ann) where
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

data Alias ann = Alias
  { aliasId :: TypeId,
    aliasType :: ann
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Pretty ann => Pretty (Alias ann) where
  pPrint (Alias {..}) = pPrint aliasId <+> equals <+> pPrint aliasType

-- *** Function

data Function ty tm = Function
  { functionType :: FunctionType ty,
    functionBody :: Term tm
  }
  deriving (Eq, Show)

instance Functor (Ty Function tm) where
  fmap f (Ty fun@Function {..}) =
    Ty fun {functionType = f <$> functionType}

instance Foldable (Ty Function tm) where
  foldMap f (Ty Function {..}) = foldMap f functionType

instance Traversable (Ty Function tm) where
  traverse f (Ty fun@Function {..}) =
    fmap Ty $
      (\functionType' -> fun {functionType = functionType'})
        <$> (f `traverse` functionType)

instance Functor (Tm Function ty) where
  fmap f (Tm fun@Function {..}) =
    Tm fun {functionBody = f <$> functionBody}

instance Foldable (Tm Function ty) where
  foldMap f (Tm Function {..}) = foldMap f functionBody

instance Traversable (Tm Function ty) where
  traverse f (Tm fun@Function {..}) =
    Tm
      . (\functionBody' -> fun {functionBody = functionBody'})
      <$> f
        `traverse` functionBody

instance (Pretty ty, Pretty tm) => Pretty (Function ty tm) where
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
      FunctionType {..} = functionType

      parameters :: (Pretty a, Pretty b) => [(a, b)] -> Doc
      parameters params = tuple $ params <&> \(a, b) -> pPrint a <+> colon <+> pPrint b

      tuple :: [Doc] -> Doc
      tuple = parens . hcat . punctuate (comma <> space)

data FunctionType ann = FunctionType
  { functionId :: TermId,
    functionIsTransform :: Bool,
    functionParameters :: [(TermId, ann)],
    -- | since contextual parameters must have newtypes, data Term only need to
    -- store the newtypes' ids
    functionContextualParameters :: Maybe [(TypeId, TermId)],
    functionOutput :: ann
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
    where
      tuple :: [Doc] -> Doc
      tuple = parens . hcat . punctuate (comma <> space)

instance Pretty ann => Pretty (FunctionType ann) where
  pPrint FunctionType {..} =
    hsep
      [ pPrint functionId,
        if functionIsTransform then "transform" else mempty,
        parameters functionParameters,
        case functionContextualParameters of
          Nothing -> mempty
          Just cxparams -> "given" <+> parameters cxparams
      ]
    where
      parameters :: (Pretty a, Pretty b) => [(a, b)] -> Doc
      parameters params = tuple $ params <&> \(a, b) -> pPrint a <+> colon <+> pPrint b

      tuple :: [Doc] -> Doc
      tuple = parens . hcat . punctuate (comma <> space)

-- *** Constant

data Constant ty tm = Constant
  { constantId :: TermId,
    constantBody :: Term tm,
    constantType :: ty
  }
  deriving (Eq, Show)

instance Functor (Ty Constant tm) where
  fmap f (Ty con@Constant {..}) = Ty con {constantType = f constantType}

instance Foldable (Ty Constant tm) where
  foldMap f (Ty Constant {..}) = f constantType

instance Traversable (Ty Constant tm) where
  traverse f (Ty con@Constant {..}) =
    Ty . (\constantType' -> con {constantType = constantType'})
      <$> f constantType

instance Functor (Tm Constant ty) where
  fmap f (Tm con@Constant {..}) = Tm con {constantBody = f <$> constantBody}

instance Foldable (Tm Constant ty) where
  foldMap f (Tm Constant {..}) = foldMap f constantBody

instance Traversable (Tm Constant ty) where
  traverse f (Tm con@Constant {..}) =
    Tm
      . (\constantBody' -> con {constantBody = constantBody'})
      <$> f
        `traverse` constantBody

instance Pretty (Constant ty tm) where
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
  | TermLet {termId :: Maybe TermId, termTerm :: Term ann, termBody :: Term ann, termAnn :: ann}
  | TermAssert {termTerm :: Term ann, termBody :: Term ann, termAnn :: ann}
  | TermStructure {termStructureId :: TypeId, termFields :: [(FieldId, Term ann)], termAnn :: ann}
  | TermMember {termTerm :: Term ann, termFieldId :: FieldId, termAnn :: ann}
  | TermProtoNeutral {termProtoNeutral :: ProtoNeutral ann, termAnn :: ann}
  | TermNeutral {termNeutral :: Neutral ann, termAnn :: ann}
  | TermAscribe {termTerm :: Term ann, termType :: Type, termAnn :: ann}
  | TermMatch {termTerm :: Term ann, termBranches :: Branches ann, termAnn :: ann}
  deriving (Eq, Show, Functor, Foldable, Traversable)

type Branches ann = [Branch ann]

type Branch ann = (Pattern ann, Term ann)

instance Pretty (Term ann) where
  pPrint = \case
    TermLiteral {..} -> pPrint termLiteral
    TermPrimitive {..} -> pPrint termPrimitive
    TermLet {..} ->
      text "let" <+> maybe "_" pPrint termId <+> equals <+> pPrint termTerm <+> semi <+> pPrint termBody
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
    -- TermNeutral {termApplicant = Applicant Nothing termId _, termMaybeArgs, termMaybeCxargs} ->
    --   ( pPrint termId
    --       <> ( case termMaybeArgs of
    --              Nothing -> mempty
    --              Just args -> parens . hcat . punctuate (comma <> space) $ pPrint <$> args
    --          )
    --   )
    --     <+> ( case termMaybeCxargs of
    --             Nothing -> mempty
    --             Just cxargs -> "giving" <+> (parens . hcat . punctuate (comma <> space) $ pPrint <$> cxargs)
    --         )
    -- TermNeutral {termApplicant = Applicant (Just typeId) termId _, termMaybeArgs, termMaybeCxargs} ->
    --   ( (pPrint typeId <> "#" <> pPrint termId)
    --       <> ( case termMaybeArgs of
    --              Nothing -> mempty
    --              Just args -> parens . hcat . punctuate (comma <> space) $ pPrint <$> args
    --          )
    --   )
    --     <+> ( case termMaybeCxargs of
    --             Nothing -> mempty
    --             Just cxargs -> "giving" <+> (parens . hcat . punctuate (comma <> space) $ pPrint <$> cxargs)
    --         )
    TermProtoNeutral {..} -> pPrint termProtoNeutral
    TermNeutral {..} -> pPrint termNeutral
    TermAscribe {..} ->
      pPrint termTerm <+> colon <+> pPrint termType
    TermMatch {..} ->
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

-- ** Neutral and Applicant

-- *** ProtoNeutral

-- should ONLY exist at parse-time
data ProtoNeutral ann = ProtoNeutral
  { protoNeutralProtoApplicant :: ProtoApplicant ann,
    protoNeutralMaybeArgs :: Maybe [Term ann],
    protoNeutralMaybeCxargs :: Maybe [Term ann]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data ProtoApplicant ann = ProtoApplicant
  { protoApplicantMaybeTypeId :: Maybe TypeId,
    protoApplicantTermId :: TermId
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Pretty (ProtoNeutral ann) where
  pPrint ProtoNeutral {..} =
    ( pPrint protoNeutralProtoApplicant
        <> case protoNeutralMaybeArgs of
          Nothing -> mempty
          Just args -> parens (commaList $ pPrint <$> args)
    )
      <+> case protoNeutralMaybeCxargs of
        Nothing -> mempty
        Just cxargs -> "giving" <+> parens (commaList $ pPrint <$> cxargs)

instance Pretty (ProtoApplicant ann) where
  pPrint ProtoApplicant {..} = case protoApplicantMaybeTypeId of
    Nothing -> pPrint protoApplicantTermId
    Just tyId -> pPrint tyId <> "#" <> pPrint protoApplicantTermId

-- *** Neutral

data Neutral ann
  = NeutralFunctionApplication {neutralFunctionId :: TermId, neutralArgs :: [Term ann], neutralMaybeCxargs :: Maybe [Term ann]}
  | NeutralEnumConstruction {neutralEnumId :: TypeId, neutralConstructorId :: TermId}
  | NeutralVariantConstruction {neutralVariantId :: TypeId, neutralConstructorId :: TermId, neutralArgs :: [Term ann]}
  | NeutralNewtypeConstruction {neutralNewtypeId :: TypeId, neutralConstructorId :: TermId, neutralArg :: Term ann}
  | Neutral {neutralTermId :: TermId}
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Pretty (Neutral ann) where
  pPrint (NeutralFunctionApplication ti tes m_tes) = (pPrint ti <> parens (commaList (pPrint <$> tes))) <+> maybe mempty (\tes' -> "giving" <+> parens (commaList (pPrint <$> tes'))) m_tes
  pPrint (NeutralEnumConstruction ti ti') = pPrint ti <> "#" <> pPrint ti'
  pPrint (NeutralVariantConstruction ti ti' tes) = pPrint ti <> "#" <> pPrint ti' <> parens (commaList (pPrint <$> tes))
  pPrint (NeutralNewtypeConstruction ti ti' te) = pPrint ti <> "#" <> pPrint ti' <> parens (pPrint te)
  pPrint (Neutral ti) = pPrint ti

-- ** Applicant

-- | `applicantOutputAnn` is the output annotation (e.g. type) of a neutral form
-- of that has this as its applicant.
data Applicant ann
  = ApplicantFunction {applicantFunctionId :: TermId, applicantOutputAnn :: ann}
  | ApplicantEnumConstructor {applicantEnumId :: TypeId, applicantConstructorId :: TermId, applicantOutputAnn :: ann}
  | ApplicantVariantConstructor {applicantVariantId :: TypeId, applicantConstructorId :: TermId, applicantOutputAnn :: ann}
  | ApplicantNewtypeConstructor {applicantNewtypeId :: TypeId, applicantConstructorId :: TermId, applicantOutputAnn :: ann}
  | Applicant {applicantTermId :: TermId, applicantOutputAnn :: ann}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Pretty (Applicant ann) where
  pPrint (ApplicantFunction ti _ann) = pPrint ti
  pPrint (ApplicantEnumConstructor ti ti' _ann) = pPrint ti <> "#" <> pPrint ti'
  pPrint (ApplicantVariantConstructor ti ti' _ann) = pPrint ti <> "#" <> pPrint ti'
  pPrint (ApplicantNewtypeConstructor ti ti' _ann) = pPrint ti <> "#" <> pPrint ti'
  pPrint (Applicant ti _ann) = pPrint ti

fromApplicantToProtoApplicant :: Applicant ann -> ProtoApplicant ann'
fromApplicantToProtoApplicant (ApplicantFunction ti _) = ProtoApplicant {protoApplicantMaybeTypeId = Nothing, protoApplicantTermId = ti}
fromApplicantToProtoApplicant (ApplicantEnumConstructor ti ti' _) = ProtoApplicant {protoApplicantMaybeTypeId = Just ti, protoApplicantTermId = ti'}
fromApplicantToProtoApplicant (ApplicantVariantConstructor ti ti' _) = ProtoApplicant {protoApplicantMaybeTypeId = Just ti, protoApplicantTermId = ti'}
fromApplicantToProtoApplicant (ApplicantNewtypeConstructor ti ti' _) = ProtoApplicant {protoApplicantMaybeTypeId = Just ti, protoApplicantTermId = ti'}
fromApplicantToProtoApplicant (Applicant ti _at) = ProtoApplicant {protoApplicantMaybeTypeId = Nothing, protoApplicantTermId = ti}

-- ** Primitive

data Primitive ann
  = PrimitiveTry (Term ann)
  | PrimitiveCast (Term ann)
  | PrimitiveNone
  | PrimitiveSome (Term ann)
  | PrimitiveTuple [Term ann]
  | PrimitiveArray [Term ann]
  | PrimitiveIf (Term ann) (Term ann) (Term ann)
  | PrimitiveAnd (Term ann) (Term ann)
  | PrimitiveOr (Term ann) (Term ann)
  | PrimitiveNot (Term ann)
  | PrimitiveEq (Term ann) (Term ann)
  | PrimitiveAdd (Term ann) (Term ann)
  | PrimitiveExtends (Term ann) TypeId
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Pretty (Primitive ann) where
  pPrint = \case
    PrimitiveTry tm -> "try" <> parens (pPrint tm)
    PrimitiveCast tm -> "cast" <> parens (pPrint tm)
    PrimitiveNone -> "None"
    PrimitiveSome tm -> "Some" <> parens (pPrint tm)
    PrimitiveTuple tms -> parens . hcat . punctuate (comma <> space) . fmap pPrint $ tms
    PrimitiveArray tms -> braces . hcat . punctuate (comma <> space) . fmap pPrint $ tms
    PrimitiveIf tm1 tm2 tm3 -> parens $ hsep ["if", pPrint tm1, "then", pPrint tm2, "else", pPrint tm3]
    PrimitiveAnd tm1 tm2 -> parens $ hsep [pPrint tm1, "&&", pPrint tm2]
    PrimitiveOr tm1 tm2 -> parens $ hsep [pPrint tm1, "||", pPrint tm2]
    PrimitiveNot tm -> "!" <> pPrint tm
    PrimitiveEq tm1 tm2 -> parens $ pPrint tm1 <+> "==" <+> pPrint tm2
    PrimitiveAdd tm1 tm2 -> parens $ pPrint tm1 <+> "+" <+> pPrint tm2
    PrimitiveExtends tm tyId -> parens $ pPrint tm <+> "extends" <+> pPrint tyId

-- ** Pattern

-- data Pattern ann
--   = PatternNamed TermId ann
--   | PatternDiscard ann
--   | PatternConstructor (Maybe TypeId) TermId [Pattern ann] ann
--   deriving (Eq, Show, Functor, Foldable, Traversable)

-- instance Pretty (Pattern ann) where
--   pPrint = \case
--     PatternNamed tmId _ -> pPrint tmId
--     PatternDiscard _ -> "_"
--     PatternConstructor tyId tmId pat _ -> (pPrint tyId <> "#" <> pPrint tmId) <+> parens (pPrint pat)

data Pattern ann
  = PatternConstructor TypeId TermId [TermId] ann
  | PatternSome TermId ann
  | PatternNone ann
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Pretty (Pattern ann) where
  pPrint = \case
    PatternConstructor tyId tmId tmIds _ -> (pPrint tyId <> "#" <> pPrint tmId) <+> parens (commaList $ pPrint <$> tmIds)
    PatternSome tmId _ -> "Some" <+> parens (pPrint tmId)
    PatternNone _ -> "None"

-- ** Type

data Type
  = TypeNumber NumberType Integer
  | TypeBit
  | TypeChar
  | TypeArray Type
  | TypeTuple [Type]
  | TypeOptional Type
  | TypeNamed TypeId
  | -- | Introduce and eliminated during typing.
    TypeUnifyVar UnifyVar (Maybe UnifyConstraint)
  deriving (Eq, Show)

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
  TermProtoNeutral {..} ->
    let ProtoNeutral {..} = termProtoNeutral
        ProtoApplicant {..} = protoNeutralProtoApplicant
     in TermProtoNeutral
          { termProtoNeutral =
              termProtoNeutral
                { protoNeutralProtoApplicant = case protoApplicantMaybeTypeId of
                    Nothing
                      | Just tmId' <- tmIds Map.!? protoApplicantTermId ->
                          ProtoApplicant
                            { protoApplicantMaybeTypeId = Nothing,
                              protoApplicantTermId = tmId'
                            }
                    _ -> protoNeutralProtoApplicant,
                  protoNeutralMaybeArgs = renameTerm tmIds <$$> protoNeutralMaybeArgs,
                  protoNeutralMaybeCxargs = renameTerm tmIds <$$> protoNeutralMaybeCxargs
                },
            termAnn
          }
  TermNeutral {..} -> case termNeutral of
    (NeutralFunctionApplication ti tes m_tes) -> term {termNeutral = NeutralFunctionApplication ti (renameTerm tmIds <$> tes) (renameTerm tmIds <$$> m_tes)}
    (NeutralEnumConstruction {}) -> term
    (NeutralVariantConstruction ti ti' tes) -> term {termNeutral = NeutralVariantConstruction ti ti' (renameTerm tmIds <$> tes)}
    (NeutralNewtypeConstruction ti ti' te) -> term {termNeutral = NeutralNewtypeConstruction ti ti' (renameTerm tmIds te)}
    (Neutral tmId)
      | Just tmId' <- tmIds Map.!? tmId -> term {termNeutral = Neutral tmId'}
      | otherwise -> term
  TermAscribe te ty r -> TermAscribe (renameTerm tmIds te) ty r
  TermMatch te branches r -> TermMatch (renameTerm tmIds te) (second (renameTerm tmIds) <$> branches) r

renamePrimitive :: Map.Map TermId TermId -> Primitive r -> Primitive r
renamePrimitive tmIds prim = case prim of
  PrimitiveTry te -> PrimitiveTry (renameTerm tmIds te)
  PrimitiveCast te -> PrimitiveCast (renameTerm tmIds te)
  PrimitiveNone -> PrimitiveNone
  PrimitiveSome tm -> PrimitiveSome (renameTerm tmIds tm)
  PrimitiveTuple tes -> PrimitiveTuple (renameTerm tmIds <$> tes)
  PrimitiveArray tes -> PrimitiveArray (renameTerm tmIds <$> tes)
  PrimitiveIf te te' te_r -> PrimitiveIf (renameTerm tmIds te) (renameTerm tmIds te') (renameTerm tmIds te_r)
  PrimitiveAnd te te' -> PrimitiveAnd (renameTerm tmIds te) (renameTerm tmIds te')
  PrimitiveOr te te' -> PrimitiveOr (renameTerm tmIds te) (renameTerm tmIds te')
  PrimitiveNot te -> PrimitiveNot (renameTerm tmIds te)
  PrimitiveEq te te' -> PrimitiveEq (renameTerm tmIds te) (renameTerm tmIds te')
  PrimitiveAdd te te' -> PrimitiveAdd (renameTerm tmIds te) (renameTerm tmIds te')
  PrimitiveExtends tm tyId -> PrimitiveExtends (renameTerm tmIds tm) tyId

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
