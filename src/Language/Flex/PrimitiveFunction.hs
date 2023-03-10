module Language.Flex.PrimitiveFunction where

data PrimitiveFunction
  = Try -- try #1
  | Cast -- cast #1
  | MakeTuple -- (#1, #2, ..., #n)
  | MakeArray -- [#1, #2, ..., #n]
  | If -- if #1 then #2 else #3
  | And -- #1 && #2
  | Or -- #1 || #2
  | Not -- ~ #1
  deriving (Show)