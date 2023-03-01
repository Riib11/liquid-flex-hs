{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Flex.Shallow where

import qualified Data.Map as Map
import Data.Text (Text, pack)
import Flex.Lexing
import Flex.Parsing
import Flex.Syntax
import Text.Parsec
import Utility

-- Module

module_ :: String -> [IO Import] -> [IO Declaration] -> IO Module
module_ str_moduleId moduleImports decls = do
  moduleId <- runShallowParser parseModuleId str_moduleId
  moduleImports <- sequence moduleImports
  moduleDeclarations <- sequence decls
  return Module {moduleId, moduleImports, moduleDeclarations}

-- Import

importOpened :: String -> IO Import
importOpened str_moduleId = ImportOpened <$> runShallowParser parseModuleId str_moduleId

importAliased :: String -> String -> IO Import
importAliased str_moduleId strAlias = ImportAliased <$> runShallowParser parseModuleId str_moduleId <*> pure (pack strAlias)

importQual :: String -> [QualImport] -> IO Import
importQual str_moduleId qualImps = ImportQual <$> runShallowParser parseModuleId str_moduleId <*> return qualImps

-- Declaration

structure :: String -> Bool -> Maybe String -> [(Text, IO Type)] -> Maybe (IO Refinement) -> IO Structure
structure str_x structureIsMessage mb_str_y ls_structureFields mb_structureRefinement = do
  structureName <- runShallowParser parseCapName str_x
  structureExtensionId <-
    maybe
      (return Nothing)
      ((Just <$>) . runShallowParser parseQualCapId)
      mb_str_y
  structureFields <- Map.fromList <$> secondM id `traverse` ls_structureFields
  structureRefinement <- maybe (return trivialRefinement) id mb_structureRefinement
  return Structure {structureName, structureModuleId = topModuleId, structureIsMessage, structureExtensionId, structureFields, structureRefinement, structureAnnotations = []}

enumerated :: String -> IO Type -> [(Text, Literal)] -> IO Enumerated
enumerated str_x io_ty ls_cases = do
  enumeratedName <- runShallowParser parseCapName str_x
  enumeratedLiteralType <- io_ty
  let enumeratedConstructors = Map.fromList ls_cases
  return Enumerated {enumeratedName, enumeratedModuleId = topModuleId, enumeratedLiteralType, enumeratedConstructors, enumeratedAnnotations = []}

variant :: String -> [(Text, IO Type)] -> IO Variant
variant str_x constrs = do
  variantName <- runShallowParser parseCapName str_x
  variantConstructors <- Map.fromList <$> secondM id `traverse` constrs
  return Variant {variantName, variantModuleId = topModuleId, variantConstructors, variantnAnnotations = []}

newtype_ :: String -> Bool -> String -> IO Type -> Maybe (IO Refinement) -> IO Newtype
newtype_ str_x newtypeIsMessage str_fieldName io_ty mb_io_rfn = do
  newtypeName <- runShallowParser parseCapName str_x
  newtypeFieldName <- runShallowParser parseName str_fieldName
  newtypeType <- io_ty
  newtypeRefinement <- maybe (return trivialRefinement) id mb_io_rfn
  return Newtype {newtypeName, newtypeModuleId = topModuleId, newtypeIsMessage, newtypeFieldName, newtypeType, newtypeRefinement, newtypeAnnotations = []}

alias :: String -> IO Type -> IO Alias
alias str_x io_ty = do
  aliasName <- runShallowParser parseCapName str_x
  aliasType <- io_ty
  return Alias {aliasName, aliasModuleId = topModuleId, aliasType, aliasAnnotations = []}

function :: String -> Bool -> IO FunctionType -> IO Term -> IO Function
function str_x functionIsTransform io_funTy io_tm = do
  functionName <- runShallowParser parseName str_x
  functionType <- io_funTy
  functionBody <- DefinitionBodyTerm <$> io_tm
  return Function {functionName, functionModuleId = topModuleId, functionIsTransform, functionType, functionBody, functionAnnotations = []}

constant :: String -> IO Type -> IO Term -> IO Constant
constant str_x io_ty io_tm = do
  constantName <- runShallowParser parseName str_x
  constantType <- io_ty
  constantBody <- DefinitionBodyTerm <$> io_tm
  return Constant {constantName, constantModuleId = topModuleId, constantType, constantBody, constantAnnotations = []}

-- Refinement

refinement :: IO Term -> IO Refinement
refinement = (Refinement . Just <$>)

-- FunctionType

functionTy :: [(Maybe Text, IO Type)] -> [(Text, IO Type)] -> IO Type -> IO FunctionType
functionTy ls_params ls_cxparams io_ty = do
  functionTypeParams <- secondM id `traverse` ls_params
  functionTypeContextualParams <- Map.fromList <$> secondM id `traverse` ls_cxparams
  functionTypeOutput <- io_ty
  return FunctionType {functionTypeParams, functionTypeContextualParams, functionTypeOutput}

-- Type

tyInt :: Integer -> IO Type
tyInt = return . TypeInt . IntSize

tyUInt :: Integer -> IO Type
tyUInt = return . TypeUInt . UIntSize

tyFloat :: Integer -> IO Type
tyFloat 32 = return $ TypeFloat FloatSize32
tyFloat 64 = return $ TypeFloat FloatSize64
tyFloat _ = error "tyFloat expects: 32, 64"

tyBit :: IO Type
tyBit = return TypeBit

tyChar :: IO Type
tyChar = return TypeChar

tyString :: IO Type
tyString = return (TypeArray TypeChar)

tyArray :: IO Type -> IO Type
tyArray = (TypeArray <$>)

tyTuple :: [IO Type] -> IO Type
tyTuple ls_io_ty = TypeTuple <$> sequence ls_io_ty

tyOptional :: IO Type -> IO Type
tyOptional = (TypeOptional <$>)

tyNamed :: String -> IO Type
tyNamed str_x = TypeNamed <$> runShallowParser parseQualCapId str_x

tyCast :: IO Type -> IO Type
tyCast = (TypeCast <$>)

-- Term

tmInt :: Integer -> IO Term
tmInt = return . fromPreterm . TermLiteral . LiteralInteger

tmFloat :: Double -> IO Term
tmFloat = return . fromPreterm . TermLiteral . LiteralFloat

tmBit :: Bool -> IO Term
tmBit = return . fromPreterm . TermLiteral . LiteralBit

tmChar :: Char -> IO Term
tmChar = return . fromPreterm . TermLiteral . LiteralChar

tmString :: Text -> IO Term
tmString = return . fromPreterm . TermLiteral . LiteralString

tmCast :: IO Term -> IO Term
tmCast = (fromPreterm . TermCast <$>)

tmVar :: String -> IO Term
tmVar str_x = fromPreterm . TermNamed <$> runShallowParser parseQualId str_x

tmTup :: [IO Term] -> IO Term
tmTup ls_io_tm = fromPreterm . TermTuple <$> sequence ls_io_tm

tmArr :: [IO Term] -> IO Term
tmArr ls_io_tm = fromPreterm . TermArray <$> sequence ls_io_tm

tmBlock :: ([IO Statement], IO Term) -> IO Term
tmBlock (ls_io_stmt, io_tm) = do
  stmts <- sequence ls_io_stmt
  tm <- io_tm
  return . fromPreterm $ TermBlock (stmts, tm)

tmStruct :: String -> [(Text, IO Term)] -> IO Term
tmStruct str_x ls_fields =
  fromPreterm `comp2` TermStructure
    <$> runShallowParser parseQualCapId str_x
    <*> (Map.fromList <$> secondM id `traverse` ls_fields)

tmMem :: IO Term -> Text -> IO Term
tmMem tm txt = fromPreterm `comp2` TermMember <$> tm <*> return txt

tmCnstr :: String -> Maybe (IO Term) -> IO Term
tmCnstr str_x mb_io_tm = do
  fromPreterm `comp2` TermConstructor
    <$> runShallowParser parseQualCapId str_x
    <*> sequence mb_io_tm

tmApp :: String -> [IO Term] -> Maybe [IO Term] -> IO Term
tmApp str_x ls_io_tms mb_io_tms =
  fromPreterm `comp3` TermApplication
    <$> runShallowParser parseQualId str_x
    <*> sequence ls_io_tms
    <*> maybe (return Nothing) ((Just . Left <$>) . sequence) mb_io_tms

tmAppPrimFun :: PrimFun -> [IO Term] -> IO Term
tmAppPrimFun pf ls_io_tms =
  fromPreterm `comp3` TermApplication
    <$> return (idOfPrimFun pf)
    <*> sequence ls_io_tms
    <*> return Nothing

tmIf :: IO Term -> IO Term -> IO Term -> IO Term
tmIf io_tm1 io_tm2 io_tm3 = fromPreterm `comp3` TermIf <$> io_tm1 <*> io_tm2 <*> io_tm3

tmAsc :: IO Term -> IO Type -> IO Term
tmAsc io_tm io_ty = fromPreterm `comp2` TermAscribe <$> io_tm <*> io_ty

-- Pattern

-- TODO: impl

-- Statement

-- TODO: impl

stmtLet :: Text -> IO Term -> IO Statement
stmtLet txt_x io_tm =
  StatementLet
    (fromPrepattern . PatternNamed $ txt_x)
    Nothing
    <$> io_tm

stmtAssert :: IO Term -> IO Statement
stmtAssert io_tm = StatementAssert <$> io_tm

-- utility

runShallowParser :: Parser a -> String -> IO a
runShallowParser p str =
  runParserT p (emptyEnv topModuleId) "shallow" str >>= \case
    Left err -> fail $ "shallow parsing error: " <> show err
    Right a -> return a
