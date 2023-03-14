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

parseModuleFile :: FilePath -> IO (Either ParseError (Module ()))
parseModuleFile fp = do
  src <- readFile fp
  runParserT (parseModule <* eof) emptyLexingEnv fp src

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
  comments
  reserved "module"
  moduleId <- parseModuleId
  reserved "where"
  moduleDeclarations <- concat <$> many parseDeclaration
  return
    Module
      { moduleId,
        moduleDeclarations
      }

-- ** Declaration

parseDeclaration :: Parser [Declaration ()]
parseDeclaration = do
  comments
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
  structureIsMessage <-
    try $
      choice
        [ reserved "message" $> True,
          reserved "struct" $> False
        ]
  structureId <- parseTypeId
  structureMaybeExtensionId <- optionMaybe (reserved "extends" *> parseTypeId)
  symbol_ "{"
  (refinedTypeRefinement, structureFields) <-
    first andRefinements . partitionEithers
      <$> (many . choice)
        [ do
            reserved "assert"
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
            structureMaybeExtensionId,
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
  try $ reserved "newtype"
  newtypeId <- parseTypeId
  symbol "{"
  (refinedTypeRefinement, newtypeFields) <-
    first andRefinements . partitionEithers
      <$> (many . choice)
        [ do
            reserved "assert"
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
    _ -> unexpected "additional field in newtype"
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
  try $ reserved "variant"
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
  try $ reserved "enum"
  enumId <- parseTypeId
  enumType <- parseType
  symbol "{"
  enumConstructors <- many do
    constrId <- parseTermId
    equals
    lit <- parseLiteral
    semi
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
  try $ reserved "type"
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
  functionIsTransform <-
    try $
      choice
        [ reserved "transform" $> True,
          reserved "function" $> False
        ]
  functionId <- parseTermId
  functionParameters <- parens $ flip sepBy comma do
    tmId <- parseTermId
    colon
    ty <- parseType
    return (tmId, ty)
  functionContextualParameters <- optionMaybe do
    reserved "given"
    parens $ flip sepBy comma do
      tmId <- parseContextualParameterId
      colon
      tyId <- parseTypeId
      return (tyId, tmId)
  reservedOp "->"
  functionOutput <- parseType
  functionBody <- parseTermBlock
  return . pure $
    toDeclaration
      Function
        { functionId,
          functionParameters,
          functionContextualParameters,
          functionIsTransform,
          functionOutput,
          functionBody
        }

parseConstant :: Parser [Declaration ()]
parseConstant = do
  try $ reserved "const"
  constantId <- parseTermId
  colon
  ty <- parseType
  equals
  constantTerm <- parseTerm
  return . pure $
    toDeclaration
      Constant
        { constantId,
          constantType = ty,
          constantTerm
        }

-- ** Type

parseType :: Parser Type
parseType =
  choice
    [ do
        try $ string "int"
        i <- integer
        return $ TypeNumber TypeInt i,
      do
        try $ string "uint"
        i <- integer
        return $ TypeNumber TypeUInt i,
      do
        try $ string "float"
        i <- integer
        return $ TypeNumber TypeFloat i,
      do
        try $ reserved "bit"
        return TypeBit,
      do
        try $ reserved "char"
        return TypeChar,
      do
        try $ reserved "Array"
        ty <- angles parseType
        return $ TypeArray ty,
      do
        try $ reserved "Tuple"
        tys <- angles (commaSep parseType)
        return $ TypeTuple tys,
      do
        try $ reserved "Optional"
        ty <- angles parseType
        return $ TypeOptional ty,
      do
        tyId <- parseTypeId
        return $ TypeNamed tyId
    ]

-- ** Term

parseTerm :: Parser (Term ())
parseTerm = buildExpressionParser table (k_term0 =<< term1) <?> "term"
  where
    -- Term#0 ::=
    -- - «TermMember»
    -- - «TermAscribe»
    -- - «Term#1»
    k_term0 :: Term () -> Parser (Term ())
    k_term0 tm = do
      choice
        [ do
            fieldId <- try (dot *> parseFieldId)
            k_term0 $ TermMember tm fieldId (),
          do
            try colon
            ty <- parseType
            k_term0 $ TermAscribe tm ty (),
          return tm
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
          do
            symbol_ "("
            tm <- parseTerm
            optionMaybe comma
              >>= \case
                Nothing -> do
                  symbol_ ")"
                  return tm
                Just _ -> do
                  tms <- commaSep parseTerm
                  symbol_ ")"
                  return $
                    TermPrimitive
                      { termPrimitive = PrimitiveTuple (tm : tms),
                        termAnn = ()
                      },
          -- starts with "{".
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
            try $ reserved "cast"
            tm <- parseTerm
            return $
              TermPrimitive
                { termPrimitive = PrimitiveCast tm,
                  termAnn = ()
                },
          -- starts with "try"
          do
            try $ reserved "try"
            tm <- parseTerm
            return $
              TermPrimitive
                { termPrimitive = PrimitiveTry tm,
                  termAnn = ()
                },
          -- starts with "if"
          do
            try $ reserved "if"
            tm1 <- parseTerm
            reserved "then"
            tm2 <- parseTerm
            reserved "else"
            tm3 <- parseTerm
            return $
              TermPrimitive
                { termPrimitive = PrimitiveIf tm1 tm2 tm3,
                  termAnn = ()
                },
          -- starts with special string
          do
            termLiteral <- try parseLiteral
            return $
              TermLiteral
                { termLiteral,
                  termAnn = ()
                },
          -- starts with «TypeId»
          try do
            termStructureId <- try parseTypeId
            termFields <- bracesTryOpen $ semiSep do
              tmId <- parseFieldId
              equals
              tm <- parseTerm
              return (tmId, tm)
            return $
              TermStructure
                { termStructureId,
                  termFields,
                  termAnn = ()
                },
          -- starts with «TermId»
          try do
            termId <- try parseTermId
            termMaybeArgs <- optionMaybe . parensTryOpen $ do
              commaSep parseTerm
            termMaybeCxargs <- optionMaybe do
              try $ reserved "giving"
              parens $ commaSep parseTerm
            return $
              TermNeutral
                { termId,
                  termMaybeArgs,
                  termMaybeCxargs,
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
          try $ reservedOp str
          return \tm -> TermPrimitive (constr tm) ()

        makeInfix constr str assoc = flip Infix assoc do
          try $ reservedOp str
          return \tm1 tm2 -> TermPrimitive (constr tm1 tm2) ()

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
        reserved "_"
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
        reserved "let"
        pat <- parsePattern
        choice
          [ do
              try colon
              ty <- parseType
              equals
              tm <- parseTerm
              semi
              return $ StatementLet pat (TermAscribe tm ty ()),
            do
              try equals
              tm <- parseTerm
              semi
              return $ StatementLet pat tm
          ],
      do
        reserved "assert"
        tm <- parseTerm
        semi
        return $ StatementAssert tm
    ]

-- ** Literal

parseLiteral :: Parser Literal
parseLiteral =
  choice
    [ LiteralFloat <$> try float,
      LiteralInteger <$> integer,
      LiteralBit <$> bitLiteral,
      LiteralChar <$> charLiteral,
      LiteralString <$> stringLiteral
    ]
