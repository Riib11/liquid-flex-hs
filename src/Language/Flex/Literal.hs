module Language.Flex.Literal where

data Literal
  = Integer Integer
  | Float Float
  | Bool Bool
  | Char Char
  | String String
  deriving (Show)