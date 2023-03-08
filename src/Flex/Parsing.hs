{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Flex.Parsing where

import Control.Lens
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text, pack)
import Flex.Lexing
import Flex.Syntax
import Text.Parsec as Parsec
import Text.Parsec.Expr as ParsecExpr
import Utility

runParser :: String -> Parser a -> String -> IO a
runParser label parser string =
  runParserT parser (emptyEnv topModuleId) (label <> "(" <> string <> ")") string >>= \case
    Left err -> error $ "parse error: " <> show err
    Right a -> return a

parseModuleId :: Parser ModuleId
parseModuleId = ModuleId <$> qualCapIdentifierTexts

parseQualId :: Parser Id
parseQualId =
  qualIdentifierTexts >>= \txts_ -> case Utility.unsnoc txts_ of
    Nothing -> unexpected "impossible: qualIdentifierTexts should not return empty list"
    Just ([], txt) -> return $ Id Nothing txt
    Just (txts, txt) -> return $ Id (Just $ ModuleId txts) txt

parseQualCapId :: Parser Id
parseQualCapId =
  qualCapIdentifierTexts >>= \txts_ -> case Utility.unsnoc txts_ of
    Nothing -> unexpected "impossible: parseQualCapId should not return empty list"
    Just ([], txt) -> return $ Id Nothing txt
    Just (txts, txt) -> return $ Id (Just $ ModuleId txts) txt

parseUnqualId :: Parser Id
parseUnqualId = Id Nothing <$> parseName

parseUnqualCapId :: Parser Id
parseUnqualCapId = Id Nothing <$> parseCapName

parseCapName :: Parser Text
parseCapName = capitalizedIdentifierText

parseName :: Parser Text
parseName = identifierText

parseNameMaybe :: Parser (Maybe Text)
parseNameMaybe = identifierTextMaybe

-- ** module

parseModule :: Parser Module
parseModule = do
  symbol_ "module"
  moduleId <- parseModuleId
  moduleImports <- many parseImport
  moduleDeclarations <- many parseDeclaration
  return Module {moduleId, moduleImports, moduleDeclarations}

parseImport :: Parser Import
parseImport = do
  symbol_ "import"
  moduleId <- parseModuleId
  choice
    [ do
        symbol_ "as"
        ImportAliased moduleId <$> parseName,
      do
        qualImps <- parens (parseQualImport `sepBy` symbol_ ",")
        return $ ImportQual moduleId qualImps,
      return $ ImportOpened moduleId
    ]

parseQualImport :: Parser QualImport
parseQualImport =
  choice
    [ QualImportConstant <$ symbol_ "const" <*> parseName,
      QualImportFunction <$ symbol_ "fun" <*> parseName,
      QualImportStructure <$ symbol_ "struct" <*> parseName,
      QualImportEnumerated <$ symbol_ "enum" <*> parseName,
      QualImportVariant <$ symbol_ "data" <*> parseName,
      QualImportAlias <$ symbol_ "alias" <*> parseName,
      QualImportNewtype <$ symbol_ "newtype" <*> parseName
    ]

parseDeclaration :: Parser Declaration
parseDeclaration = do
  moduleId <- (^. envModuleId) <$> Parsec.getState
  choice
    [ do
        symbol_ "struct"
        structureIsMessage <- isJust <$> optionMaybe (symbol_ "message")
        structureName <- parseCapName
        structureExtensionId <- optionMaybe (symbol "extends" *> parseQualCapId)
        symbol_ "{"
        -- structureFields <- parseMap parseName (symbol_ ":") parseType (symbol_ ";")
        structureFields <-
          Map.fromList <$> many do
            n <- parseName
            symbol_ ":"
            ty <- parseType
            symbol_ ";"
            return (n, ty)
        structureRefinement <- fromMaybe trivialRefinement <$> optionMaybe (try parseRefinement)
        symbol_ "}"
        structureAnnotations <- parseAnnotations
        return $ DeclarationStructure Structure {structureName, structureModuleId = moduleId, structureIsMessage, structureExtensionId, structureFields, structureRefinement, structureAnnotations},
      do
        symbol_ "enum"
        enumeratedName <- parseCapName
        symbol_ "="
        enumeratedLiteralType <- parseType
        symbol_ "{"
        enumeratedConstructors <- parseMap parseCapName (symbol_ "=") parseLiteral (symbol_ "|")
        symbol_ "}"
        enumeratedAnnotations <- parseAnnotations
        return $ DeclarationEnumerated Enumerated {enumeratedName, enumeratedModuleId = moduleId, enumeratedLiteralType, enumeratedConstructors, enumeratedAnnotations},
      do
        symbol_ "data"
        variantName <- parseCapName
        symbol_ "{"
        variantConstructors <- parseMap parseCapName (symbol_ ":") parseType (symbol_ "|")
        symbol_ "}"
        variantnAnnotations <- parseAnnotations
        return $ DeclarationVariant Variant {variantName, variantModuleId = moduleId, variantConstructors, variantnAnnotations},
      do
        symbol_ "newtype"
        newtypeIsMessage <- isJust <$> optionMaybe (symbol_ "message")
        newtypeName <- parseCapName
        symbol_ "{"
        newtypeFieldName <- parseName
        symbol_ ":"
        newtypeType <- parseType
        newtypeRefinement <- fromMaybe trivialRefinement <$> optionMaybe (try (symbol_ ";" *> parseRefinement))
        symbol_ "}"
        newtypeAnnotations <- parseAnnotations
        return $ DeclarationNewtype Newtype {newtypeName, newtypeModuleId = moduleId, newtypeIsMessage, newtypeFieldName, newtypeType, newtypeRefinement, newtypeAnnotations},
      do
        symbol_ "alias"
        aliasName <- parseCapName
        symbol_ "="
        aliasType <- parseType
        aliasAnnotations <- parseAnnotations
        return $ DeclarationAlias Alias {aliasName, aliasModuleId = moduleId, aliasType, aliasAnnotations},
      do
        symbol_ "fun"
        functionIsTransform <- isJust <$> optionMaybe (symbol_ "transform")
        functionName <- parseName
        functionType <- parseFunctionType
        functionBody <- parseDefinitionBody
        functionAnnotations <- parseAnnotations
        return $ DeclarationFunction Function {functionName, functionModuleId = moduleId, functionIsTransform, functionType, functionBody, functionAnnotations},
      do
        symbol_ "const"
        constantName <- parseName
        symbol_ ":"
        constantType <- parseType
        constantBody <- parseDefinitionBody
        constantAnnotations <- parseAnnotations
        return $ DeclarationConstant Constant {constantName, constantModuleId = moduleId, constantType, constantBody, constantAnnotations}
    ]

parseRefinement :: Parser Refinement
parseRefinement = Refinement . Just <$> (reserved "@assert" *> parens parseTerm)

parseFunctionType :: Parser FunctionType
parseFunctionType = do
  functionTypeParams <- parens $ parseTuple parseNameMaybe (symbol ":") parseType `sepBy` symbol ","
  functionTypeContextualParams <- fromMaybe Map.empty <$> optionMaybe (mapFromUniqueList =<< braces (parseTuple parseName (symbol ":") parseType `sepBy` symbol ","))
  symbol_ "->"
  functionTypeOutput <- parseType
  return FunctionType {functionTypeParams, functionTypeContextualParams, functionTypeOutput}

parseType :: Parser Type
parseType =
  choice
    [ TypeInt . IntSize <$ symbol_ "int" <*> natural,
      TypeUInt . UIntSize <$ symbol_ "uint" <*> natural,
      TypeFloat <$ symbol_ "float" <*> choice [FloatSize32 <$ symbol "32", FloatSize64 <$ symbol "64"],
      TypeBit <$ symbol_ "bit",
      TypeChar <$ symbol_ "char",
      TypeArray <$> brackets parseType,
      TypeTuple <$> parens (parseType `sepBy` symbol ","),
      TypeOptional <$ symbol "optional" <*> angles parseType,
      TypeNamed <$> parseQualCapId
    ]

parseTerm :: Parser Term
parseTerm = buildExpressionParser table term0 <?> "term"
  where
    term0 :: Parser Term
    term0 = do
      tm <- term1
      let h :: Parser Preterm -> Parser Term
          h m = try (k . fromPreterm =<< m)

          k :: Term -> Parser Term
          k tm' =
            choice
              [ h $ TermMember tm' <$> (symbol "." *> parseName),
                h $ TermAscribe tm' <$> (symbol ":" *> parseType),
                return tm'
              ]
      k tm

    term1 =
      choice
        ( try (parens parseTerm)
            : (<$$>)
              fromPreterm
              [ TermLiteral <$> parseLiteral,
                -- begin with: parens
                try parseTermTuple,
                -- begin with: bracketes
                TermArray <$> brackets (parseTerm `sepBy` symbol ","),
                -- begin with: braces
                TermBlock <$> braces ((,) <$> many (parseStatement <* symbol ";") <*> parseTerm),
                -- begin with: parseQualId
                try (TermCast <$> (symbol "cast" *> parseTerm)),
                try (TermStructure <$> parseQualCapId <*> braces (parseMap parseName (symbol "=") parseTerm (symbol ";"))),
                try (TermConstructor <$> parseQualCapId <*> optionMaybe (parens parseTerm)),
                try (TermConstructor <$> parseQualCapId <*> optionMaybe (fromPreterm <$> parseTermTuple)),
                try
                  ( TermApplication
                      <$> parseQualId
                      <*> parens (parseTerm `sepBy` symbol ",")
                      <*> choice
                        [ Just . Left <$> (symbol "given" *> parens (parseTerm `sepBy` symbol ",")),
                          return Nothing
                        ]
                  ),
                TermIf <$ symbol "if" <*> parseTerm <* symbol "then" <*> parseTerm <* symbol "else" <*> parseTerm,
                TermNamed <$> parseQualId
              ]
        )
        <?> "simple term"

    parseTermTuple = TermTuple <$> parens (parseTerm `sepBy` symbol ",")

    table =
      [ [prefixPrimFun PrimFunNot],
        [binaryPrimFun PrimFunAnd AssocLeft, binaryPrimFun PrimFunOr AssocLeft],
        [binaryPrimFun PrimFunEq AssocNone]
      ]

    prefixPrimFun pf = Prefix do
      void $ symbol (stringOfPrimFun pf)
      return (fromPreterm . termApp1 (idOfPrimFun pf))

    binaryPrimFun pf assoc = flip Infix assoc do
      void $ symbol (stringOfPrimFun pf)
      return (\a b -> fromPreterm $ termApp2 (idOfPrimFun pf) a b)

{-
-- parses that can immediately begin with a parse of index > 0
parseTerm :: Parser Term
parseTerm = do
  tm <- parseTerm_1
  let h :: Parser Preterm -> Parser Term
      h m = try (k . fromPreterm =<< m)

      k :: Term -> Parser Term
      k tm' =
        let parseInfixPrimFun :: PrimFun -> Parser Term
            parseInfixPrimFun pf = h $ termApp2 (idOfPrimFun pf) tm' <$> (symbol (stringOfPrimFun pf) *> parseTerm)
         in choice
              [ h $ TermMember tm' <$> (symbol "." *> parseName),
                h $ TermAscribe tm' <$> (symbol ":" *> parseType),
                parseInfixPrimFun PrimFunEq,
                parseInfixPrimFun PrimFunOr,
                parseInfixPrimFun PrimFunAnd,
                return tm'
              ]
  k tm

-- parses that can immediately begin with a parse of index > 1
parseTerm_1 :: Parser Term
parseTerm_1 =
  choice . concat $
    [ [try (parens parseTerm)],
      (fromPreterm <$$>) $
        [ TermLiteral <$> parseLiteral,
          TermCast <$ symbol_ "cast" <*> parens parseTerm,
          parsePrefixPrimFun PrimFunNot,
          -- begin with: parens
          try parseTermTuple,
          -- begin with: bracketes
          TermArray <$> brackets (parseTerm `sepBy` symbol ","),
          -- begin with: braces
          TermBlock <$> braces ((,) <$> many (parseStatement <* symbol ";") <*> parseTerm),
          -- begin with: parseQualId
          try (TermStructure <$> parseQualCapId <*> braces (parseMap parseName (symbol "=") parseTerm (symbol ";"))),
          try (TermConstructor <$> parseQualCapId <*> optionMaybe (parens parseTerm)),
          try (TermConstructor <$> parseQualCapId <*> optionMaybe (fromPreterm <$> parseTermTuple)),
          try
            ( TermApplication
                <$> parseQualId
                <*> parens (parseTerm `sepBy` symbol ",")
                <*> choice
                  [ Just . Left <$> (symbol "given" *> parens (parseTerm `sepBy` symbol ",")),
                    return Nothing
                  ]
            ),
          TermIf <$ symbol "if" <*> parseTerm <* symbol "then" <*> parseTerm <* symbol "else" <*> parseTerm,
          TermNamed <$> parseQualId
        ]
    ]
  where
    parseTermTuple = TermTuple <$> parens (parseTerm `sepBy` symbol ",")

    parsePrefixPrimFun pf = do
      void $ symbol (stringOfPrimFun pf)
      termApp1 (idOfPrimFun pf) <$> parseTerm
-}

-- TODO: handle other kinds of definition bodies
parseDefinitionBody :: Parser DefinitionBody
parseDefinitionBody =
  choice
    [ DefinitionBodyTerm <$> parseTerm,
      DefinitionBodyTerm <$> (symbol_ "=" *> parseTerm)
    ]

parseLiteral :: Parser Literal
parseLiteral =
  choice
    [ LiteralInteger <$> integer,
      LiteralFloat <$> float,
      LiteralBit <$> choice [True <$ reserved "true", False <$ reserved "false"],
      LiteralChar <$> charLiteral,
      LiteralString <$> (pack <$> stringLiteral)
    ]

parseAnnotations :: Parser [Annotation]
parseAnnotations = return []

parsePattern :: Parser Pattern
parsePattern =
  choice . (fromPrepattern <$$>) $
    [ -- try $ PatternAscribe <$> parsePattern <* symbol ":" <*> parseType, -- TODO: unroll recursion
      -- try $ PatternExtends <$> parsePattern <* symbol "extends" <*> parseQualId, -- TODO: unroll recursion
      PatternDiscard <$ symbol "_",
      PatternLiteral <$> parseLiteral,
      -- PatternTuple <$> parens (parsePattern `sepBy` symbol ","),
      -- PatternArray <$> braces (parsePattern `sepBy` symbol ","),
      -- PatternField <$> parsePattern <*> parseName, -- TODO: unroll recursion
      -- PatternConstructor <$> parseQualId <*> parens parsePattern,
      PatternNamed <$> parseName
    ]

-- TODO: should the unif type's name just be "let"?
parseStatement :: Parser Statement
parseStatement =
  choice
    [ try do
        -- annotating a `let` is the same as ascribing its imp
        symbol_ "let"
        pat <- parsePattern
        symbol_ ":"
        sig <- parseType
        symbol_ "="
        imp <- parseTerm
        return $ StatementLet pat (fromPreterm $ TermAscribe imp sig),
      StatementLet
        <$ symbol "let"
        <*> parsePattern
        -- <*> choice
        --   [ colon *> (Just <$> parseType),
        --     return Nothing
        --   ]
        -- <* symbol "="
        <*> parseTerm,
      StatementAssert <$ symbol "assert" <*> parens parseTerm
    ]

-- ** comments

parseCommentLines :: Parser ()
parseCommentLines = void $ many do
  void $ string "--"
  void $ manyTill anyChar newline

-- * utilities

parseTuple :: Parser a -> Parser bet -> Parser b -> Parser (a, b)
parseTuple a bet b = (,) <$> a <* bet <*> b

parseMap :: Ord key => Parser key -> Parser bet -> Parser val -> Parser sep -> Parser (Map.Map key val)
parseMap key bet val sep = Map.fromList <$> ((,) <$> key <* bet <*> val) `sepBy` sep

mapFromUniqueList :: [(Text, v)] -> Parser (Map.Map Text v)
mapFromUniqueList vs = do
  let m = Map.fromList vs
  unless (Map.size m == length vs) $
    unexpected $
      "expected labels to be unique among: " <> show (fst <$> vs)
  return m

-- mapFromUniqueListHas_id :: Show a => Has_id a => [a] -> Parser (Map.Map Id a)
-- mapFromUniqueListHas_id as = do
--   let m = Map.fromList $ (\d -> (get_id d, d)) <$> as
--   unless (Map.size m < length as) $
--     unexpected $ "expected ids to be unique among: " <> show as
--   return $ m
