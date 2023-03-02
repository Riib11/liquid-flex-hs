{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Flex.Syntax where

import Control.Lens
import Control.Monad.State (execState)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import qualified Flex.Unif as Unif
import PrettyShow
import Utility

-- * syntax

-- ** identifier

data ModuleId = ModuleId [Text] deriving (Eq, Ord, Show)

instance PrettyShow ModuleId where
  prettyShow (ModuleId txts) = List.intercalate "." (unpack <$> txts)

topModuleId :: ModuleId
topModuleId = ModuleId []

fromUnqualText :: Text -> Id
fromUnqualText = Id Nothing

tryUnqualify :: Id -> Maybe Text
tryUnqualify (Id mb_mdlId txt) = case mb_mdlId of
  Nothing -> return txt
  Just _ -> Nothing

data Id = Id (Maybe ModuleId) Text deriving (Eq, Ord, Show)

-- TODO: choose actual convention for this
newtypeElementId :: Id
newtypeElementId = fromUnqualText (pack "newtypeElementId")

instance PrettyShow Id where
  prettyShow (Id mb_mdlId txt) = maybe "" ((<> ".") . prettyShow) mb_mdlId <> unpack txt

-- ** module

data Module = Module
  { moduleId :: ModuleId,
    moduleImports :: [Import],
    moduleDeclarations :: [Declaration]
  }
  deriving (Eq, Show)

instance PrettyShow Module where
  prettyShow mdl =
    List.intercalate "\n\n" . concat $
      [ [unwords ["module", prettyShow (moduleId mdl)]],
        prettyShow <$> moduleImports mdl,
        prettyShow <$> moduleDeclarations mdl
      ]

-- ** import

data Import
  = ImportOpened ModuleId
  | ImportAliased ModuleId Text
  | ImportQual ModuleId [QualImport]
  deriving (Eq, Show)

instance PrettyShow Import where
  prettyShow = \case
    ImportOpened mdlId -> unwords ["import", prettyShow mdlId]
    ImportAliased mdlId txt -> unwords ["import", prettyShow mdlId, "as", prettyShow txt]
    ImportQual mdlId quals -> unwords ["import", prettyShow mdlId, "(", List.intercalate ", " (prettyShow <$> quals), ")"]

data QualImport
  = QualImportConstant Text
  | QualImportFunction Text
  | QualImportStructure Text
  | QualImportEnumerated Text
  | QualImportVariant Text
  | QualImportAlias Text
  | QualImportNewtype Text
  deriving (Eq, Show)

instance PrettyShow QualImport where
  prettyShow = \case
    QualImportConstant txt -> unwords ["constant", prettyShow txt]
    QualImportFunction txt -> unwords ["function", prettyShow txt]
    QualImportStructure txt -> unwords ["structure", prettyShow txt]
    QualImportEnumerated txt -> unwords ["enumerated", prettyShow txt]
    QualImportVariant txt -> unwords ["variant", prettyShow txt]
    QualImportAlias txt -> unwords ["alias", prettyShow txt]
    QualImportNewtype txt -> unwords ["newtype", prettyShow txt]

-- ** declaration

data Declaration
  = DeclarationStructure Structure
  | DeclarationNewtype Newtype
  | DeclarationVariant Variant
  | DeclarationEnumerated Enumerated
  | DeclarationAlias Alias
  | DeclarationFunction Function
  | DeclarationConstant Constant
  deriving (Eq, Show)

data DeclarationType
  = DeclarationTypeStructure Structure
  | DeclarationTypeNewtype Newtype
  | DeclarationTypeVariant Variant
  | DeclarationTypeEnumerated Enumerated
  | DeclarationTypeAlias Alias
  deriving (Eq, Show)

instance PrettyShow Declaration where
  prettyShow = \case
    DeclarationStructure d -> prettyShow d
    DeclarationNewtype d -> prettyShow d
    DeclarationVariant d -> prettyShow d
    DeclarationEnumerated d -> prettyShow d
    DeclarationAlias d -> prettyShow d
    DeclarationFunction d -> prettyShow d
    DeclarationConstant d -> prettyShow d

instance PrettyShow DeclarationType where
  prettyShow = \case
    DeclarationTypeStructure a -> prettyShow a
    DeclarationTypeNewtype a -> prettyShow a
    DeclarationTypeVariant a -> prettyShow a
    DeclarationTypeEnumerated a -> prettyShow a
    DeclarationTypeAlias a -> prettyShow a

-- ** structure

data Structure = Structure
  { structureName :: Text,
    structureModuleId :: ModuleId,
    structureIsMessage :: Bool,
    structureExtensionId :: Maybe Id,
    structureFields :: Map.Map Text Type,
    structureRefinement :: Refinement,
    structureAnnotations :: [Annotation]
  }
  deriving (Show)

instance Eq Structure where (==) = namedEq

instance Ord Structure where (<=) = namedOrd

instance PrettyShow Structure where
  prettyShow struct =
    unwords . concat $
      [ ["struct"],
        if structureIsMessage struct then ["message"] else [],
        [prettyShow $ structureName struct],
        maybe [] (\x -> ["extends " <> prettyShow x]) (structureExtensionId struct),
        ["{"],
        [unwords ((\(txt, ty) -> unpack txt <> ": " <> prettyShow ty <> ";") <$> Map.toList (structureFields struct))],
        [prettyShow $ structureRefinement struct],
        ["}"]
      ]

-- ** enumerated

data Enumerated = Enumerated
  { enumeratedName :: Text,
    enumeratedModuleId :: ModuleId,
    enumeratedLiteralType :: Type,
    enumeratedConstructors :: Map.Map Text Literal,
    enumeratedAnnotations :: [Annotation]
  }
  deriving (Show)

instance Eq Enumerated where (==) = namedEq

instance Ord Enumerated where (<=) = namedOrd

instance PrettyShow Enumerated where
  prettyShow enm =
    unwords . concat $
      [ ["enum", prettyShow $ enumeratedName enm, "=", prettyShow $ enumeratedLiteralType enm],
        ["{"],
        [unwords [prettyShow $ txt, "=", prettyShow ty] | (txt, ty) <- Map.toList $ enumeratedConstructors enm],
        ["}"]
      ]

-- ** variant

data Variant = Variant
  { variantName :: Text,
    variantModuleId :: ModuleId,
    variantConstructors :: Map.Map Text Type,
    variantnAnnotations :: [Annotation]
  }
  deriving (Show)

instance Eq Variant where (==) = namedEq

instance Ord Variant where (<=) = namedOrd

instance PrettyShow Variant where
  prettyShow varnt =
    unwords . concat $
      [ ["data", prettyShow $ variantName varnt, "="],
        [ List.intercalate " | " [prettyShow txt, ":", prettyShow ty]
          | (txt, ty) <- Map.toList $ variantConstructors varnt
        ]
      ]

-- ** newtype

data Newtype = Newtype
  { newtypeName :: Text,
    newtypeModuleId :: ModuleId,
    newtypeIsMessage :: Bool,
    newtypeFieldName :: Text,
    newtypeType :: Type,
    newtypeRefinement :: Refinement,
    newtypeAnnotations :: [Annotation]
  }
  deriving (Show)

instance Eq Newtype where (==) = namedEq

instance Ord Newtype where (<=) = namedOrd

instance PrettyShow Newtype where
  prettyShow newty =
    unwords . concat $
      [ ["newtype"],
        if newtypeIsMessage newty then ["message"] else [],
        [prettyShow $ newtypeName newty],
        ["{"],
        [(prettyShow $ newtypeFieldName newty) <> ":", (prettyShow $ newtypeType newty) <> ";"],
        [prettyShow $ newtypeRefinement newty],
        ["}"]
      ]

-- ** alias

data Alias = Alias
  { aliasName :: Text,
    aliasModuleId :: ModuleId,
    aliasType :: Type,
    aliasAnnotations :: [Annotation]
  }
  deriving (Show)

instance Eq Alias where (==) = namedEq

instance Ord Alias where (<=) = namedOrd

instance PrettyShow Alias where
  prettyShow alias =
    unwords $
      [ "alias",
        prettyShow $ aliasName alias,
        "=",
        prettyShow $ aliasType alias
      ]

-- ** function

data Function = Function
  { functionName :: Text,
    functionModuleId :: ModuleId,
    functionIsTransform :: Bool,
    functionType :: FunctionType,
    functionBody :: DefinitionBody,
    functionAnnotations :: [Annotation]
  }
  deriving (Show)

instance Eq Function where (==) = namedEq

instance Ord Function where (<=) = namedOrd

instance PrettyShow Function where
  prettyShow fun =
    unwords . concat $
      [ ["fun"],
        if functionIsTransform fun then ["transform"] else [],
        [prettyShow $ functionName fun],
        [prettyShow $ functionType fun],
        [prettyShow $ functionBody fun]
      ]

-- ** constant

data Constant = Constant
  { constantName :: Text,
    constantModuleId :: ModuleId,
    constantType :: Type,
    constantBody :: DefinitionBody,
    constantAnnotations :: [Annotation]
  }
  deriving (Show)

instance Eq Constant where (==) = namedEq

instance Ord Constant where (<=) = namedOrd

instance PrettyShow Constant where
  prettyShow con =
    unwords
      [ "const",
        prettyShow $ constantName con,
        ":",
        prettyShow $ constantType con,
        "=",
        prettyShow $ constantBody con
      ]

-- *** Has_name

class Has_name a where
  get_name :: a -> Text

instance Has_name Structure where
  get_name = structureName

instance Has_name Enumerated where
  get_name = enumeratedName

instance Has_name Variant where
  get_name = variantName

instance Has_name Alias where
  get_name = aliasName

instance Has_name Newtype where
  get_name = newtypeName

instance Has_name Function where
  get_name = functionName

instance Has_name Constant where
  get_name = constantName

instance Has_name Declaration where
  get_name = \case
    DeclarationStructure d -> get_name d
    DeclarationNewtype d -> get_name d
    DeclarationVariant d -> get_name d
    DeclarationEnumerated d -> get_name d
    DeclarationAlias d -> get_name d
    DeclarationFunction d -> get_name d
    DeclarationConstant d -> get_name d

-- *** Has_moduleId

class Has_moduleId a where
  get_moduleId :: a -> ModuleId

instance Has_moduleId Structure where
  get_moduleId = structureModuleId

instance Has_moduleId Enumerated where
  get_moduleId = enumeratedModuleId

instance Has_moduleId Variant where
  get_moduleId = variantModuleId

instance Has_moduleId Alias where
  get_moduleId = aliasModuleId

instance Has_moduleId Newtype where
  get_moduleId = newtypeModuleId

instance Has_moduleId Function where
  get_moduleId = functionModuleId

instance Has_moduleId Constant where
  get_moduleId = constantModuleId

instance Has_moduleId Declaration where
  get_moduleId = \case
    DeclarationStructure d -> get_moduleId d
    DeclarationNewtype d -> get_moduleId d
    DeclarationVariant d -> get_moduleId d
    DeclarationEnumerated d -> get_moduleId d
    DeclarationAlias d -> get_moduleId d
    DeclarationFunction d -> get_moduleId d
    DeclarationConstant d -> get_moduleId d

-- *** NamedEq

-- Equality by checking name info

namedEq :: (Has_moduleId a, Has_name a) => a -> a -> Bool
namedEq x y = get_moduleId x == get_moduleId y && get_name x == get_name y

namedOrd :: (Has_moduleId a, Has_name a) => a -> a -> Bool
namedOrd x y = get_moduleId x <= get_moduleId y && get_name x == get_name y

-- *** Has_isMessage

class Has_isMessage a where
  get_isMessage :: a -> Bool

instance Has_isMessage Structure where
  get_isMessage = structureIsMessage

instance Has_isMessage Newtype where
  get_isMessage = newtypeIsMessage

-- ** refinement

data Refinement = Refinement (Maybe Term)
  deriving (Eq, Show)

trivialRefinement :: Refinement
trivialRefinement = Refinement Nothing

andRefinement :: Refinement -> Refinement -> Refinement
andRefinement = unimplemented "andRefinement"

ifRefinement :: [(Refinement, Refinement)] -> Refinement
ifRefinement = unimplemented "orRefinement"

eqRefinement :: Term -> Term -> Refinement
eqRefinement tm1 tm2 =
  Refinement . Just $
    Term
      { _termPreterm = TermApplication (fromUnqualText $ nameOfPrimFun PrimFunEq) [tm1, tm2] Nothing,
        _termType = Just TypeBit
      }

instance PrettyShow Refinement where
  prettyShow (Refinement (Just tm)) = "@assert(" <> prettyShow tm <> ")"
  prettyShow (Refinement Nothing) = "@assert(True)"

-- ** annotation

data Annotation
  deriving (Eq, Ord, Show)

-- ** definition body

data DefinitionBody
  = DefinitionBodyTerm Term
  | DefinitionBodyExternal Text
  | DefinitionBodyPrimFun PrimFun
  | DefinitionBodyPrimConst PrimConst
  | DefinitionBodyDerived (Maybe Term)
  deriving (Eq, Show)

instance PrettyShow DefinitionBody where
  prettyShow = \case
    DefinitionBodyTerm tm -> prettyShow tm
    DefinitionBodyExternal txt -> "external " <> prettyShow txt
    DefinitionBodyPrimFun pf -> "primitive function " <> stringOfPrimFun pf
    DefinitionBodyPrimConst pc -> "primitive constant " <> stringOfPrimConst pc
    DefinitionBodyDerived mb_tm -> maybe "derived" prettyShow mb_tm

-- ** function type

data FunctionType = FunctionType
  { functionTypeParams :: [(Maybe Text, Type)],
    functionTypeContextualParams :: Map.Map Text Type,
    functionTypeOutput :: Type
  }
  deriving (Eq, Show)

instance PrettyShow FunctionType where
  prettyShow funTy =
    unwords . concat $
      [ ["(" <> List.intercalate "; " ((\(mb_txt, ty) -> maybe "_" prettyShow mb_txt <> ": " <> prettyShow ty) <$> functionTypeParams funTy) <> ")"],
        ["{" <> List.intercalate ", " ((\(txt, ty) -> prettyShow txt <> ": " <> prettyShow ty) <$> Map.toList (functionTypeContextualParams funTy)) <> "}" | not (Map.null (functionTypeContextualParams funTy))],
        ["->"],
        [prettyShow (functionTypeOutput funTy)]
      ]

-- ** type

data Type
  = TypeInt IntSize
  | TypeUInt UIntSize
  | TypeFloat FloatSize
  | TypeBit
  | TypeChar
  | TypeArray Type
  | TypeTuple [Type]
  | TypeOptional Type
  | TypeNamed Id
  | -- | wraps a type, indicating that it can be cast to an expected type if
    -- there is a cast from the wrapped type to that type; doesn't appear in
    -- user syntax
    TypeCast Type
  | -- | type unification variable; doesn't appear in user syntax
    TypeUnif Unif.Id
  | -- | TypeStructure, TypeEnumerated, TypeVariant, TypeNewtype don't appear in
    -- user syntax
    TypeStructure Structure
  | TypeEnumerated Enumerated
  | TypeVariant Variant
  | TypeNewtype Newtype
  deriving (Eq, Ord, Show)

isLiteralType :: Type -> Bool
isLiteralType = \case
  TypeInt _ -> True
  TypeUInt _ -> True
  TypeFloat _ -> True
  TypeChar -> True
  TypeArray TypeChar -> True
  _ -> False

instance PrettyShow Type where
  prettyShow = \case
    TypeInt s -> "int" <> prettyShow s
    TypeUInt s -> "uint" <> prettyShow s
    TypeFloat s -> "float" <> prettyShow s
    TypeBit -> "bit"
    TypeChar -> "char"
    TypeArray ty -> "[" <> prettyShow ty <> "]"
    TypeTuple tys -> "(" <> List.intercalate ", " (prettyShow <$> tys) <> ")"
    TypeOptional ty -> "optional(" <> prettyShow ty <> ")"
    TypeNamed x -> prettyShow x
    TypeCast ty -> "cast(" <> prettyShow ty <> ")"
    TypeStructure struct -> prettyShow $ structureName struct
    TypeEnumerated enm -> prettyShow $ enumeratedName enm
    TypeVariant varnt -> prettyShow $ variantName varnt
    TypeNewtype newty -> prettyShow $ newtypeName newty
    TypeUnif u -> prettyShow u

-- *** numeric sizes

newtype IntSize = IntSize Integer
  deriving (Eq, Ord, Show)

newtype UIntSize = UIntSize Integer
  deriving (Eq, Ord, Show)

data FloatSize = FloatSize32 | FloatSize64
  deriving (Eq, Ord, Show)

instance PrettyShow IntSize where prettyShow (IntSize i) = show i

instance PrettyShow UIntSize where prettyShow (UIntSize i) = show i

instance PrettyShow FloatSize where
  prettyShow = \case
    FloatSize32 -> "32"
    FloatSize64 -> "64"

-- ** term

-- type Term = (Preterm, Type)

data Term = Term
  { _termPreterm :: Preterm,
    _termType :: Maybe Type
  }
  deriving (Eq, Show)

-- TODO: add TermTry
data Preterm
  = TermLiteral Literal
  | TermCast Term
  | TermNamed Id
  | TermTuple [Term]
  | TermArray [Term]
  | TermBlock ([Statement], Term)
  | TermStructure Id (Map.Map Text Term)
  | TermMember Term Text
  | TermConstructor Id (Maybe Term)
  | TermApplication Id [Term] (Maybe (Either [Term] (Map.Map Type Term)))
  | TermIf Term Term Term
  | TermAscribe Term Type
  | TermMatch Term [(Pattern, Term)]
  deriving (Eq, Show)

termApp1 :: Id -> Term -> Preterm
termApp1 x tm = TermApplication x [tm] Nothing

termApp2 :: Id -> Term -> Term -> Preterm
termApp2 x tm1 tm2 = TermApplication x [tm1, tm2] Nothing

fromPreterm :: Preterm -> Term
fromPreterm _termPreterm = Term {_termPreterm, _termType = Nothing}

makeTerm :: Preterm -> Type -> Term
makeTerm _termPreterm ty = Term {_termPreterm, _termType = Just ty}

instance PrettyShow Term where
  prettyShow tm = prettyShow (_termPreterm tm)

instance PrettyShow Preterm where
  prettyShow = \case
    TermLiteral lit -> prettyShow lit
    TermCast tm -> "cast(" <> prettyShow tm <> ")"
    TermNamed x -> prettyShow x
    TermTuple tms -> "(" <> List.intercalate ", " (prettyShow <$> tms) <> ")"
    TermArray tms -> "[" <> List.intercalate ", " (prettyShow <$> tms) <> "]"
    TermBlock (stmts, tm) ->
      case stmts of
        [] -> "{ " <> prettyShow tm <> " }"
        _ -> "{ " <> List.intercalate "; " (prettyShow <$> stmts) <> " ; " <> prettyShow tm <> " }"
    TermStructure x fds -> prettyShow x <> " { " <> (List.intercalate "; " ((\(txt, tm) -> unpack txt <> " = " <> prettyShow tm) <$> Map.toList fds)) <> " }"
    TermMember tm txt -> prettyShow tm <> "." <> unpack txt
    TermConstructor x tm -> prettyShow x <> "(" <> prettyShow tm <> ")"
    TermApplication x args mb_cxargs ->
      prettyShow x
        <> "("
        <> List.intercalate ", " (prettyShow <$> args)
        <> ")"
        <> case mb_cxargs of
          Nothing -> ""
          Just (Left cxargsList) -> " given (" <> List.intercalate ", " (prettyShow <$> cxargsList) <> ")"
          Just (Right cxargsMap) -> " given (" <> List.intercalate ", " ((\(ty, tm) -> prettyShow tm <> ": " <> prettyShow ty) <$> Map.toList cxargsMap) <> ")"
    TermIf tmIf tmThen tmElse -> "if " <> prettyShow tmIf <> " then " <> prettyShow tmThen <> " else " <> prettyShow tmElse
    TermAscribe tm ty -> prettyShow tm <> " : " <> prettyShow ty
    TermMatch tm _branches -> "match " <> prettyShow tm <> " with " <> error "TODO: prettyShow match"

-- ** literal

data Literal
  = LiteralInteger Integer
  | LiteralFloat Double
  | LiteralBit Bool
  | LiteralChar Char
  | LiteralString Text
  deriving (Eq, Show)

instance PrettyShow Literal where
  prettyShow = \case
    LiteralInteger i -> show i
    LiteralFloat d -> show d
    LiteralBit b -> if b then "true" else "false"
    LiteralChar c -> show c
    LiteralString s -> show s

-- ** pattern

data Pattern = Pattern {_patternPrepattern :: Prepattern, _patternType :: Maybe Type}
  deriving (Eq, Show)

data Prepattern
  = -- | case of Enumerated or reference to Constant
    PatternNamed Text
  | PatternDiscard
  | PatternLiteral Literal
  {- TODO: more features
  \| PatternAscribe Pattern Type
  \| PatternExtends Pattern Id
  \| PatternTuple [Pattern]
  \| PatternArray [Pattern]
  \| PatternField Pattern Text
  \| -- | constructor or Variant or constructor of Newtype
    PatternConstructor Id Pattern
  -}
  deriving (Eq, Show)

instance PrettyShow Pattern where
  prettyShow = prettyShow . _patternPrepattern

instance PrettyShow Prepattern where
  prettyShow = \case
    PatternNamed txt -> unpack txt
    PatternDiscard -> "_"
    PatternLiteral lit -> prettyShow lit

fromPrepattern :: Prepattern -> Pattern
fromPrepattern _patternPrepattern = Pattern {_patternPrepattern, _patternType = Nothing}

-- ** statement

data Statement
  = StatementLet Pattern (Maybe Type) Term
  | StatementAssert Term
  deriving (Eq, Show)

instance PrettyShow Statement where
  prettyShow = \case
    StatementLet pat mb_ty tm -> "let " <> prettyShow pat <> (maybe "" ((": " <>) . prettyShow) mb_ty) <> " = " <> prettyShow tm
    StatementAssert tm -> "assert(" <> prettyShow tm <> ")"

-- ** primitives

-- *** primitive functions

data PrimFun
  = PrimFunEq
  | PrimFunOr
  | PrimFunAnd
  | PrimFunNot
  deriving (Eq, Ord, Enum, Bounded, Show)

arityPrimFun :: PrimFun -> Int
arityPrimFun = \case
  PrimFunEq -> 2
  PrimFunOr -> 2
  PrimFunAnd -> 2
  PrimFunNot -> 1

toPrimFun :: Text -> Maybe PrimFun
toPrimFun = \case
  "==" -> Just PrimFunEq
  "||" -> Just PrimFunOr
  "&&" -> Just PrimFunAnd
  "!" -> Just PrimFunNot
  _ -> Nothing

stringOfPrimFun :: PrimFun -> String
stringOfPrimFun = \case
  PrimFunEq -> "=="
  PrimFunOr -> "||"
  PrimFunAnd -> "&&"
  PrimFunNot -> "!"

nameOfPrimFun :: PrimFun -> Text
nameOfPrimFun = pack . stringOfPrimFun

idOfPrimFun :: PrimFun -> Id
idOfPrimFun = fromUnqualText . nameOfPrimFun

typeOfPrimFun :: PrimFun -> FunctionType
typeOfPrimFun = \case
  PrimFunEq ->
    let (ufId : _) = Unif.ids
     in help [(Just "== arg1", TypeUnif ufId), (Just "== arg2", TypeUnif ufId)] TypeBit
  PrimFunAnd -> help [(Just "&& arg1", TypeBit), (Just "&& arg2", TypeBit)] TypeBit
  PrimFunOr -> help [(Just "|| arg2", TypeBit), (Just "|| arg2", TypeBit)] TypeBit
  PrimFunNot -> help [(Just "! arg1", TypeBit)] TypeBit
  where
    help functionTypeParams functionTypeOutput = FunctionType {functionTypeParams, functionTypeContextualParams = Map.empty, functionTypeOutput}

-- *** primitive constants

data PrimConst
  = PrimConstPi
  deriving (Eq, Ord, Enum, Show)

toPrimConst :: Text -> Maybe PrimConst
toPrimConst = \case
  "pi" -> Just PrimConstPi
  _ -> Nothing

stringOfPrimConst :: PrimConst -> String
stringOfPrimConst = \case
  PrimConstPi -> "pi"

nameOfPrimConst :: PrimConst -> Text
nameOfPrimConst = pack . stringOfPrimConst

idOfPrimConst :: PrimConst -> Id
idOfPrimConst = fromUnqualText . nameOfPrimConst

-- *** primitive constructors

data PrimCnstr
  = PrimCnstrNone
  | PrimCnstrSome
  deriving (Eq, Ord, Enum)

-- ** lenses

makeLenses ''Term
makeLenses ''Pattern

-- ** module context

data ModuleCtx = ModuleCtx
  { _ctxModuleId :: ModuleId,
    -- contents are organized by namedspace
    _ctxModuleTypes :: Map.Map Id DeclarationType,
    _ctxModuleFunctions :: Map.Map Id Function,
    _ctxModuleConstants :: Map.Map Id Constant,
    _ctxModuleConstructors :: Map.Map Id Constructor
  }
  deriving (Eq, Show)

data Constructor
  = ConstructorNewtype Newtype
  | ConstructorVariant Variant Type
  | ConstructorEnumerated Enumerated Literal
  deriving (Eq, Show)

makeLenses ''ModuleCtx

instance PrettyShow ModuleCtx where
  prettyShow mdlCtx =
    unlines . concat $
      [ [unwords [prettyShow x, "::", prettyShow a] | (x, a) <- Map.toList (mdlCtx ^. ctxModuleTypes)],
        [unwords [prettyShow x, "::", prettyShow a] | (x, a) <- Map.toList (mdlCtx ^. ctxModuleFunctions)],
        [unwords [prettyShow x, "::", prettyShow a] | (x, a) <- Map.toList (mdlCtx ^. ctxModuleConstants)],
        [unwords [prettyShow x, "::", prettyShow a] | (x, a) <- Map.toList (mdlCtx ^. ctxModuleConstructors)]
      ]

instance PrettyShow Constructor where
  prettyShow = \case
    ConstructorNewtype newty -> "constructor of newtype " <> prettyShow (newtypeName newty)
    ConstructorVariant varnt ty -> "constructor of variant " <> prettyShow (variantName varnt) <> " with argument type " <> prettyShow ty
    ConstructorEnumerated enm lit -> "constructor of enum " <> prettyShow (enumeratedName enm) <> " with value " <> prettyShow lit

topModuleCtx :: ModuleCtx
topModuleCtx =
  ModuleCtx
    { _ctxModuleId = topModuleId,
      _ctxModuleTypes = mempty,
      _ctxModuleFunctions =
        Map.fromList
          . fmap
            ( \pf ->
                ( idOfPrimFun pf,
                  Function
                    { functionName = nameOfPrimFun pf,
                      functionIsTransform = False,
                      functionModuleId = topModuleId,
                      functionType = typeOfPrimFun pf,
                      functionBody = DefinitionBodyPrimFun pf,
                      functionAnnotations = []
                    }
                )
            )
          $ (enumFromTo minBound maxBound),
      -- TODO: primitive constants
      _ctxModuleConstants = mempty,
      -- TODO: primitive constructors
      _ctxModuleConstructors = mempty
    }

toOpenedModuleCtx :: Module -> ModuleCtx
toOpenedModuleCtx mdl =
  topModuleCtx & execState do
    ctxModuleId .= moduleId mdl
    flip mapM_ (moduleDeclarations mdl) \case
      DeclarationStructure d -> do
        ctxModuleTypes %= Map.insert (fromUnqualText $ get_name d) (DeclarationTypeStructure d)
      DeclarationAlias d -> do
        ctxModuleTypes %= Map.insert (fromUnqualText $ get_name d) (DeclarationTypeAlias d)
      DeclarationVariant d -> do
        ctxModuleTypes %= Map.insert (fromUnqualText $ get_name d) (DeclarationTypeVariant d)
        flip mapM_ (Map.toList $ variantConstructors d) \(txt, ty) ->
          ctxModuleConstructors %= Map.insert (fromUnqualText txt) (ConstructorVariant d ty)
      DeclarationEnumerated d -> do
        ctxModuleTypes %= Map.insert (fromUnqualText $ get_name d) (DeclarationTypeEnumerated d)
        flip mapM_ (Map.toList $ enumeratedConstructors d) \(txt, lit) ->
          ctxModuleConstructors %= Map.insert (fromUnqualText txt) (ConstructorEnumerated d lit)
      DeclarationNewtype d -> do
        ctxModuleTypes %= Map.insert (fromUnqualText $ get_name d) (DeclarationTypeNewtype d)
        ctxModuleConstructors %= Map.insert (fromUnqualText $ newtypeFieldName d) (ConstructorNewtype d)
      DeclarationFunction d -> do
        ctxModuleFunctions %= Map.insert (fromUnqualText $ get_name d) d
      DeclarationConstant d -> do
        ctxModuleConstants %= Map.insert (fromUnqualText $ get_name d) d

toQualifiedModuleCtx :: [Import] -> Module -> ModuleCtx
toQualifiedModuleCtx = error "TODO: adds only imported ids"

toAliasedModuleCtx :: Text -> Module -> ModuleCtx
toAliasedModuleCtx = error "TODO: adds only aliased ids"

subModuleCtx :: ModuleCtx -> ModuleCtx -> ModuleCtx
subModuleCtx = error "TODO: add sub module ctx stuff to parent module ctx"
