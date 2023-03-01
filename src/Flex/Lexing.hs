{-# LANGUAGE TemplateHaskell #-}

module Flex.Lexing where

import Control.Lens
import Control.Monad
import Data.Text (Text, pack)
import Flex.Syntax
import qualified Flex.Unif as Unif
import Text.Parsec
import qualified Text.Parsec.Token as Token
import Prelude hiding (lex)

-- ** parser state

data Env = Env
  { _envUnif :: Unif.Env,
    _envModuleId :: ModuleId
  }

makeLenses ''Env

emptyEnv :: ModuleId -> Env
emptyEnv _envModuleId =
  Env
    { _envUnif = Unif.emptyEnv,
      _envModuleId
    }

freshUnifType :: String -> Parser Type
freshUnifType str = do
  eu <- (^. envUnif) <$> getState
  let (u, eu') = Unif.freshId str eu
  modifyState (envUnif .~ eu')
  return $ TypeUnif u

-- ** parser

type Parser = ParsecT String Env IO

languageDef :: Token.GenLanguageDef String Env IO
languageDef =
  Token.LanguageDef
    { Token.commentStart = "/*",
      Token.commentEnd = "*/",
      Token.commentLine = "//",
      Token.nestedComments = False,
      -- TODO: confirm selection
      Token.identStart = identStart,
      -- TODO: confirm selection
      Token.identLetter = identLetter,
      -- TODO: confirm selection
      Token.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
      -- TODO: confirm selection
      Token.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      Token.reservedNames =
        concat
          [ -- module
            ["module", "import", "as", "type"],
            -- declaration
            ["struct", "variant", "enum", "type", "newtype", "function", "const", "derive", "test"],
            -- statement
            ["assert"],
            -- type
            ["optional", "int", "uint", "float", "bit"],
            -- term
            -- "output": output of function (used in refinement types)
            -- "_": discarded id or pattern
            ["match", "with", "if", "then", "else", "given"],
            -- patterm
            ["_"],
            -- refinements
            ["@assert"]
          ],
      Token.reservedOpNames = ["=", ",", ";", ":", "."],
      Token.caseSensitive = True
    }

identStart :: Parser Char
identStart = lower

identLetter :: Parser Char
identLetter = alphaNum <|> char '_'

lexer :: Token.GenTokenParser String Env IO
lexer = Token.makeTokenParser languageDef

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

angles :: Parser a -> Parser a
angles = Token.angles lexer

identifier :: Parser String
identifier = Token.identifier lexer

identifierText :: Parser Text
identifierText = pack <$> identifier

capitalizedIdentifier :: Parser String
capitalizedIdentifier = lexeme ((:) <$> upper <*> many identLetter)

capitalizedIdentifierText :: Parser Text
capitalizedIdentifierText = pack <$> capitalizedIdentifier

qualIdentifierTexts :: Parser [Text]
qualIdentifierTexts =
  choice
    [ try ((:) <$> capitalizedIdentifierText <*> many1 identifierText),
      pure <$> identifierText
    ]

qualCapIdentifierTexts :: Parser [Text]
qualCapIdentifierTexts = capitalizedIdentifierText `sepBy` char '.'

identifierTextMaybe :: Parser (Maybe Text)
identifierTextMaybe =
  choice
    [ Just <$> identifierText,
      Nothing <$ symbol_ "_"
    ]

-- no lexeme
identifier' :: Parser String
identifier' = identStart >>= \c -> go [c]
  where
    go str =
      choice
        [ identLetter >>= \c -> go (c : str),
          return (reverse str)
        ]

identifierText' :: Parser Text
identifierText' = pack <$> identifier'

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

integer :: Parser Integer
integer = Token.integer lexer

natural :: Parser Integer
natural = Token.natural lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

float :: Parser Double
float = Token.float lexer

charLiteral :: Parser Char
charLiteral = Token.charLiteral lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

comma :: Parser String
comma = Token.comma lexer

colon :: Parser String
colon = Token.colon lexer

equal :: Parser ()
equal = reservedOp "="

divider :: Parser ()
divider = reservedOp "|"

semi :: Parser String
semi = Token.semi lexer

symbol_ :: String -> Parser ()
symbol_ = void . symbol

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer
