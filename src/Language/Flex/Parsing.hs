module Language.Flex.Parsing where

import Control.Lens
import Control.Monad
import Data.Bifunctor (Bifunctor (first))
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import Data.Maybe
import Language.Flex.Lexing
import Language.Flex.Syntax
import Text.Parsec as Parsec
import Text.Parsec.Expr as ParsecExpr
import Utility

-- * Parsing

-- ** Id

parseModuleId :: Parser ModuleId
parseModuleId = ModuleId <$> identifier

parseTypeId :: Parser TypeId
parseTypeId = TypeId <$> identifier

parseTermId :: Parser TermId
parseTermId = TermId <$> identifier

parseFieldId :: Parser FieldId
parseFieldId = FieldId <$> identifier

-- contextual parameter must begin with "?"
parseContextualParameterId :: Parser TermId
parseContextualParameterId = TermId <$> ((++) <$> string "?" <*> identifier)

-- ** Module

parseModule :: Parser (Module ())
parseModule = do
  symbol_ "module"
  moduleId <- parseModuleId
  moduleDeclarations <- concat <$> many parseDeclaration
  return
    Module
      { moduleId,
        moduleDeclarations
      }

-- ** Declaration

parseDeclaration :: Parser [Declaration ()]
parseDeclaration = do
  choice
    [ do
        symbol_ "struct"
        structureIsMessage <- isJust <$> optionMaybe (symbol_ "message")
        structureId <- parseTypeId
        structureExtensionId <- optionMaybe (symbol_ "extends" *> parseTypeId)
        symbol_ "{"
        (refinedTypeRefinement, structureFields) <-
          first andRefinements . partitionEithers
            <$> (many . choice)
              [ do
                  symbol_ "assert"
                  tm <- parseTerm
                  semi
                  return . Left $ Refinement tm,
                do
                  fieldId <- parseFieldId
                  colon
                  ty <- parseType
                  semi
                  return . Right $ (fieldId, ty)
              ]
        symbol_ "}"
        return
          [ toDeclaration
              Structure
                { structureId,
                  structureExtensionId,
                  structureIsMessage,
                  structureFields
                },
            toDeclaration
              RefinedType
                { refinedTypeId = structureId,
                  refinedTypeRefinement
                }
          ],
      do
        symbol_ "newtype"
        newtypeId <- parseTypeId
        symbol "{"
        (refinedTypeRefinement, newtypeFields) <-
          first andRefinements . partitionEithers
            <$> (many . choice)
              [ do
                  symbol_ "assert"
                  tm <- parseTerm
                  semi
                  return . Left $ Refinement tm,
                do
                  fieldId <- parseFieldId
                  colon
                  ty <- parseType
                  semi
                  return . Right $ (fieldId, ty)
              ]
        (newtypeFieldId, newtypeType) <- case newtypeFields of
          [(newtyFieldId, newtyType)] -> return (newtyFieldId, newtyType)
          _ -> unexpected "a newtype declaration must have exactly one field"
        symbol "}"
        return
          [ toDeclaration
              Newtype
                { newtypeId,
                  newtypeFieldId,
                  newtypeType
                },
            toDeclaration
              RefinedType
                { refinedTypeId = newtypeId,
                  refinedTypeRefinement
                }
          ],
      do
        symbol_ "variant"
        variantId <- parseTypeId
        symbol "{"
        variantConstructors <- many do
          tmId <- parseTermId
          mb_tyParams <- optionMaybe $ parens $ parseType `sepBy` comma
          semi
          return (tmId, mb_tyParams)
        symbol "}"
        return . pure $
          toDeclaration
            Variant
              { variantId,
                variantConstructors
              },
      do
        symbol_ "enum"
        enumeratedId <- parseTypeId
        enumeratedType <- parseType
        symbol "{"
        enumeratedConstructors <- many do
          constrId <- parseTermId
          lit <- parseLiteral
          return (constrId, lit)
        symbol "}"
        return . pure $
          toDeclaration
            Enumerated
              { enumeratedId,
                enumeratedType,
                enumeratedConstructors
              },
      do
        symbol_ "type"
        aliasId <- parseTypeId
        equals
        aliasType <- parseType
        return . pure $
          toDeclaration
            Alias
              { aliasId,
                aliasType
              },
      do
        symbol_ "transform"
        symbol_ "function"
        functionId <- parseTermId
        functionParameters <- parens $ flip sepBy comma do
          tmId <- parseTermId
          colon
          ty <- parseType
          return (tmId, ty)
        functionContextualParameters <- optionMaybe do
          symbol_ "given"
          parens $ flip sepBy comma do
            tmId <- parseContextualParameterId
            colon
            tyId <- parseTypeId
            return (tyId, tmId)
        symbol_ "->"
        functionOutput <- parseType
        functionBody <- parseBlock
        return . pure $
          toDeclaration
            Function
              { functionId,
                functionParameters,
                functionContextualParameters,
                functionIsTransform = True,
                functionOutput,
                functionBody
              },
      do
        symbol_ "constant"
        constantId <- parseTermId
        colon
        ty <- parseType
        equals
        tm <- parseTerm
        let constantTerm = TermAscribe tm ty ()
        return . pure $
          toDeclaration
            Constant
              { constantId,
                constantTerm
              }
    ]

-- ** Type

parseType :: Parser Type
parseType = undefined

-- ** Term

parseTerm :: Parser (Term ())
parseTerm = undefined

parseBlock :: Parser (Term ())
parseBlock = undefined

-- ** Pattern

parsePattern :: Parser (Pattern ())
parsePattern = undefined

-- ** Statement

parseStatement :: Parser (Statement ())
parseStatement = undefined

-- ** Literal

parseLiteral :: Parser Literal
parseLiteral = undefined
