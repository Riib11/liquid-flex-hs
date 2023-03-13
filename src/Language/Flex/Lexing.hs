{-# LANGUAGE TemplateHaskell #-}

module Language.Flex.Lexing where

import Control.Lens
import Control.Monad
import Data.Text (Text, pack)
import Text.Parsec
import qualified Text.Parsec.Token as Token
import Prelude hiding (lex)

-- ** parser state

data LexingEnv = LexingEnv
  {}

makeLenses ''LexingEnv

emptyLexingEnv :: LexingEnv
emptyLexingEnv = LexingEnv {}

-- ** parser

type Parser = ParsecT String LexingEnv IO

runParser :: SourceName -> Parser a -> String -> IO a
runParser srcName parser str =
  runParserT parser emptyLexingEnv srcName str >>= \case
    Left err -> error $ "parse error: " <> show err
    Right a -> return a

languageDef :: Token.GenLanguageDef String LexingEnv IO
languageDef =
  Token.LanguageDef
    { Token.commentStart = "/*",
      Token.commentEnd = "*/",
      Token.commentLine = "//",
      Token.nestedComments = False,
      Token.identStart = identStart,
      Token.identLetter = identLetter,
      Token.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
      Token.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      Token.reservedNames =
        concat
          [ -- module
            ["module", "import", "as"],
            -- declaration
            ["struct", "variant", "enum", "type", "newtype", "function", "const"],
            -- statement
            ["assert"],
            -- type
            ["Array", "Tuple", "Optional", "int", "uint", "float", "bit"],
            -- term
            -- "output": output of function (used in refinement types)
            -- "_": discarded id or pattern
            ["try", "cast", "match", "with", "if", "then", "else", "given"],
            -- pattern
            ["_"]
          ],
      Token.reservedOpNames = ["=", ",", ";", ":", "."],
      Token.caseSensitive = True
    }

identStart :: Parser Char
identStart = letter <|> char '?'

identLetter :: Parser Char
identLetter = alphaNum <|> char '_'

lexer :: Token.GenTokenParser String LexingEnv IO
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

identifierTextMaybe :: Parser (Maybe Text)
identifierTextMaybe =
  choice
    [ Just <$> identifierText,
      Nothing <$ symbol_ "_"
    ]

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

equals :: Parser ()
equals = reservedOp "="

divider :: Parser ()
divider = reservedOp "|"

semi :: Parser String
semi = Token.semi lexer

symbol_ :: String -> Parser ()
symbol_ = void . symbol

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer