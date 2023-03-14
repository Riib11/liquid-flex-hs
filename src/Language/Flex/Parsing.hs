{-# HLINT ignore "Use <$>" #-}

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
import Utility hiding (angles)

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
    [ parseStructure,
      parseNewtype,
      parseVariant,
      parseEnum,
      parseAlias,
      parseFunction,
      parseConstant
    ]

parseStructure :: Parser [Declaration ()]
parseStructure = do
  structureIsMessage <- isJust <$> optionMaybe (symbol_ "message")
  symbol_ "struct"
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
    ]

parseNewtype :: Parser [Declaration ()]
parseNewtype = do
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
    ]

parseVariant :: Parser [Declaration ()]
parseVariant = do
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
        }

parseEnum :: Parser [Declaration ()]
parseEnum = do
  symbol_ "enum"
  enumId <- parseTypeId
  enumType <- parseType
  symbol "{"
  enumConstructors <- many do
    constrId <- parseTermId
    lit <- parseLiteral
    return (constrId, lit)
  symbol "}"
  return . pure $
    toDeclaration
      Enum
        { enumId,
          enumType,
          enumConstructors
        }

parseAlias :: Parser [Declaration ()]
parseAlias = do
  symbol_ "type"
  aliasId <- parseTypeId
  equals
  aliasType <- parseType
  return . pure $
    toDeclaration
      Alias
        { aliasId,
          aliasType
        }

parseFunction :: Parser [Declaration ()]
parseFunction = do
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
  functionBody <- parseTermBlock
  return . pure $
    toDeclaration
      Function
        { functionId,
          functionParameters,
          functionContextualParameters,
          functionIsTransform = True,
          functionOutput,
          functionBody
        }

parseConstant :: Parser [Declaration ()]
parseConstant = do
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

-- ** Type

parseType :: Parser Type
parseType =
  choice
    [ do
        -- doesn't parse whitespace after, so that can immediately parse size
        -- integer
        numty <-
          choice
            [ string "int" $> TypeInt,
              string "uint" $> TypeUInt,
              string "float" $> TypeFloat
            ]
        i <- integer
        return $ TypeNumber numty i,
      do
        symbol_ "bit" $> TypeBit,
      do
        symbol_ "char" $> TypeChar,
      do
        symbol_ "Array"
        ty <- angles parseType
        return $ TypeArray ty,
      do
        symbol_ "Tuple"
        tys <- angles (commaSep parseType)
        return $ TypeTuple tys,
      do
        symbol_ "Optional"
        ty <- angles parseType
        return $ TypeOptional ty,
      do
        tyId <- parseTypeId
        return $ TypeNamed tyId
    ]

-- ** Term

parseTerm :: Parser (Term ())
parseTerm = buildExpressionParser table term0 <?> "term"
  where
    -- Term#0 ::=
    -- - «TermMember»
    -- - «TermAscribe»
    -- - «Term#1»
    term0 :: Parser (Term ())
    term0 = do
      tm <- term1
      choice
        [ do
            symbol_ "."
            fieldId <- parseFieldId
            return $ TermMember tm fieldId (),
          do
            symbol ":"
            ty <- parseType
            return $ TermAscribe tm ty ()
        ]

    -- «Term#1» ::=
    -- - «TermLiteral»
    -- - «TermPrimitive» (special cases)
    -- - «TermBlock»
    -- - «TermStructure»
    -- - «TermNeutral»
    -- - «TermMatch»
    -- - («Term»)
    term1 :: Parser (Term ())
    term1 =
      choice
        [ -- starts with "("
          try do
            parens parseTerm,
          do
            parens do
              tms <- commaSep parseTerm
              return $
                TermPrimitive
                  { termPrimitive = PrimitiveTuple tms,
                    termAnn = ()
                  },
          -- starts with "{"
          do
            parseTermBlock,
          -- starts with "["
          do
            brackets do
              tms <- commaSep parseTerm
              return $
                TermPrimitive
                  { termPrimitive = PrimitiveArray tms,
                    termAnn = ()
                  },
          -- starts with "cast"
          do
            symbol_ "cast"
            tm <- parseTerm
            return $
              TermPrimitive
                { termPrimitive = PrimitiveCast tm,
                  termAnn = ()
                },
          -- starts with "try"
          do
            symbol_ "try"
            tm <- parseTerm
            return $
              TermPrimitive
                { termPrimitive = PrimitiveTry tm,
                  termAnn = ()
                },
          -- starts with "if"
          do
            symbol_ "if"
            tm1 <- parseTerm
            symbol_ "then"
            tm2 <- parseTerm
            symbol_ "else"
            tm3 <- parseTerm
            return $
              TermPrimitive
                { termPrimitive = PrimitiveIf tm1 tm2 tm3,
                  termAnn = ()
                },
          -- starts with «TypeId»
          try do
            termStructureId <- parseTypeId
            termFields <- braces $ many do
              tmId <- parseFieldId
              equals
              tm <- parseTerm
              semi
              return (tmId, tm)
            return $
              TermStructure
                { termStructureId,
                  termFields,
                  termAnn = ()
                },
          -- starts with «TermId»
          try do
            termId <- parseTermId
            termMaybeArgs <- optionMaybe . parens $ do
              commaSep parseTerm
            termMaybeCxargs <- optionMaybe do
              symbol_ "giving"
              parens $ commaSep parseTerm
            return $
              TermNeutral
                { termId,
                  termMaybeArgs,
                  termMaybeCxargs,
                  termAnn = ()
                },
          try do
            termLiteral <- parseLiteral
            return $
              TermLiteral
                { termLiteral,
                  termAnn = ()
                }
        ]

    table :: OperatorTable String LexingEnv IO (Term ())
    table =
      [ [makePrefix PrimitiveNot "!"],
        [makeInfix PrimitiveAnd "&&" AssocLeft],
        [makeInfix PrimitiveOr "||" AssocLeft],
        [makeInfix PrimitiveEq "==" AssocNone]
      ]
      where
        makePrefix constr str = Prefix do
          symbol_ str
          return \tm -> TermPrimitive (constr tm) ()

        makeInfix constr str assoc = flip Infix assoc do
          symbol_ str
          return \tm1 tm2 -> TermPrimitive (constr tm1 tm2) ()

-- infix

parseBlock :: Parser (Block ())
parseBlock = braces do
  stmts <- many parseStatement
  tm <- parseTerm
  return (stmts, tm)

-- | Parses a block, but if the list of statements is empty then just yields
-- parsed term
parseTermBlock :: Parser (Term ())
parseTermBlock = braces do
  stmts <- many parseStatement
  tm <- parseTerm
  if null stmts
    then return tm
    else
      return $
        TermBlock
          { termBlock = (stmts, tm),
            termAnn = ()
          }

-- ** Pattern

parsePattern :: Parser (Pattern ())
parsePattern =
  choice
    [ do
        symbol_ "_"
        return $ PatternDiscard (),
      do
        lit <- parseLiteral
        return $ PatternLiteral lit (),
      do
        tmId <- parseTermId
        return $ PatternNamed tmId ()
    ]

-- ** Statement

parseStatement :: Parser (Statement ())
parseStatement =
  choice
    [ do
        symbol_ "let"
        pat <- parsePattern
        equals
        tm <- parseTerm
        return $ StatementLet pat tm,
      do
        symbol_ "assert"
        tm <- parseTerm
        return $ StatementAssert tm
    ]

-- ** Literal

parseLiteral :: Parser Literal
parseLiteral =
  choice
    [ LiteralInteger <$> integer,
      LiteralFloat <$> float,
      LiteralBit <$> bitLiteral,
      LiteralChar <$> charLiteral,
      LiteralString <$> stringLiteral
    ]
