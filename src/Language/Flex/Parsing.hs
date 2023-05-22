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

parseModuleFile :: FilePath -> IO (Either ParseError (Module Type ()))
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

parseModule :: Parser (Module Type ())
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

parseDeclaration :: Parser [Declaration Type ()]
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

parseStructure :: Parser [Declaration Type ()]
parseStructure = do
  structureIsMessage <-
    try $
      choice
        [ do
            reserved "message"
            optional $ reserved "struct"
            return True,
          -- reserved "message" $> True,
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

parseNewtype :: Parser [Declaration Type ()]
parseNewtype = do
  try $ reserved "newtype"
  newtypeId <- parseTypeId
  let newtypeConstructorId = fromNewtypeIdToTermId newtypeId
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
            newtypeConstructorId,
            newtypeFieldId,
            newtypeType
          },
      toDeclaration
        RefinedType
          { refinedTypeId = newtypeId,
            refinedTypeRefinement
          }
    ]

parseVariant :: Parser [Declaration Type ()]
parseVariant = do
  try $ reserved "variant"
  variantId <- parseTypeId
  symbol "{"
  variantConstructors <- many do
    tmId <- parseTermId
    mb_tyParams <- optionMaybe $ parens $ parseType `sepBy` comma
    semi
    return (tmId, fromMaybe [] mb_tyParams)
  symbol "}"
  return . pure $
    toDeclaration
      Variant
        { variantId,
          variantConstructors
        }

parseEnum :: Parser [Declaration Type ()]
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

parseAlias :: Parser [Declaration Type ()]
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

parseFunction :: Parser [Declaration Type ()]
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
    try $ reserved "given"
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
        { functionType =
            FunctionType
              { functionId,
                functionIsTransform,
                functionParameters,
                functionContextualParameters,
                functionOutput
              },
          functionBody
        }

parseConstant :: Parser [Declaration Type ()]
parseConstant = do
  try $ reserved "const"
  constantId <- parseTermId
  colon
  ty <- parseType
  equals
  constantBody <- parseTerm
  return . pure $
    toDeclaration
      Constant
        { constantId,
          constantType = ty,
          constantBody
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
        tys <- angles (commaSep1 parseType)
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
            try dot
            fieldId <- try parseFieldId
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
    -- - «TermMatch»
    -- - «TermBlock»
    -- - «TermStructure»
    -- - «TermNeutral»
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
          -- starts with "assertFalse"
          do
            try $ reserved "assertFalse"
            return $
              TermPrimitive
                { termPrimitive = PrimitiveException,
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
          -- starts with "None"
          do
            try $ reserved "None"
            return $ TermPrimitive PrimitiveNone (),
          -- starts with "Some"
          do
            try $ reserved "Some"
            tm <- parens parseTerm
            return $ TermPrimitive (PrimitiveSome tm) (),
          -- starts with "match"
          do
            try $ reserved "match"
            tm <- parseTerm
            reserved "with"
            branches <-
              braces . many $ do
                pat <- parsePattern
                symbol_ "=>"
                body <- parseTerm
                semi
                return (pat, body)

            return
              TermMatch
                { termTerm = tm,
                  termBranches = branches,
                  termAnn = ()
                },
          -- starts with other special string
          do
            termLiteral <- try parseLiteral
            return $
              TermLiteral
                { termLiteral,
                  termAnn = ()
                },
          -- starts with «TypeId»
          do
            termStructureId <- try do
              termStructureId <- parseTypeId
              symbol_ "{"
              return termStructureId
            termFields <- semiSep do
              tmId <- parseFieldId
              equals
              tm <- parseTerm
              return (tmId, tm)
            symbol "}"
            return $
              TermStructure
                { termStructureId,
                  termFields,
                  termAnn = ()
                },
          -- starts with «TermId»
          do
            (protoApplicantMaybeTypeId, protoApplicantTermId) <-
              choice
                [ do
                    tyId <- try do
                      tyId <- parseTypeId
                      hash
                      return tyId
                    tmId <- parseTermId
                    return (Just tyId, tmId),
                  do
                    tmId <- parseTermId
                    return (Nothing, tmId)
                ]
            protoNeutralMaybeArgs <-
              optionMaybe . parensTryOpen $
                commaSep parseTerm
            protoNeutralMaybeCxargs <- optionMaybe do
              try $ reserved "giving"
              parens $ commaSep parseTerm
            return $
              TermProtoNeutral
                { termProtoNeutral =
                    ProtoNeutral
                      { protoNeutralProtoApplicant =
                          ProtoApplicant
                            { protoApplicantMaybeTypeId,
                              protoApplicantTermId
                            },
                        protoNeutralMaybeArgs,
                        protoNeutralMaybeCxargs
                      },
                  termAnn = ()
                }
        ]

    table :: OperatorTable String LexingEnv IO (Term ())
    table =
      [ [makeInfix (PrimitiveNumBinOp nbo) (operatorOfNumBinOp nbo) AssocLeft | nbo <- [NumBinOpMul, NumBinOpDiv]],
        [makeInfix (PrimitiveNumBinOp nbo) (operatorOfNumBinOp nbo) AssocLeft | nbo <- [NumBinOpAdd, NumBinOpSub]],
        [makeInfix (PrimitiveNumBinOp nbo) (operatorOfNumBinOp nbo) AssocLeft | nbo <- [NumBinOpMod]],
        [makeInfix (PrimitiveNumBinRel nbr) (operatorOfNumBinRel nbr) AssocNone | nbr <- enumFrom (toEnum 0)],
        [makePrefix PrimitiveNot "!"],
        [makeInfix (PrimitiveBoolBinOp BoolBinOpAnd) "&&" AssocLeft],
        [makeInfix (PrimitiveBoolBinOp BoolBinOpOr) "||" AssocLeft],
        [makeInfix (PrimitiveEq True) "==" AssocNone, makeInfix (PrimitiveEq False) "!=" AssocNone],
        [makeInfix (PrimitiveBoolBinOp BoolBinOpImp) "==>" AssocRight]
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
parseTermBlock = braces go
  where
    go :: Parser (Term ())
    go =
      choice
        [ -- TermLet
          do
            try $ reserved "let"
            mb_tmId <-
              choice
                [ Just <$> parseTermId,
                  symbol "_" >> return Nothing
                ]

            choice
              [ do
                  try colon
                  ty <- parseType
                  equals
                  tm <- parseTerm
                  semi
                  body <- go
                  return $ TermLet mb_tmId (TermAscribe tm ty ()) body (),
                do
                  try equals
                  tm <- parseTerm
                  semi
                  body <- go
                  return $ TermLet mb_tmId tm body ()
              ],
          -- TermAssert
          do
            try $ reserved "assert"
            tm <- parseTerm
            semi
            body <- go
            return $ TermAssert tm body (),
          -- Term
          do
            parseTerm
        ]

-- ** Pattern

-- parsePattern :: Parser (Pattern ())
-- parsePattern =
--   choice
--     [ do
--         reserved "_"
--         return $ PatternDiscard (),
--       do
--         tmId <- parseTermId
--         return $ PatternNamed tmId ()
--     ]

parsePattern :: Parser (Pattern ())
parsePattern =
  choice
    [ do
        try $ reserved "None"
        return $ PatternNone (),
      do
        try $ reserved "Some"
        tmId <- parens parseTermId
        return $ PatternSome tmId (),
      do
        tyId <- try parseTypeId
        symbol_ "#"
        tmId <- parseTermId
        params <-
          choice
            [ parens $ sepBy parseTermId comma,
              return []
            ]
        return $ PatternConstructor tyId tmId params ()
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
