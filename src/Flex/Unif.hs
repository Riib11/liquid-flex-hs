{-# LANGUAGE TemplateHaskell #-}

module Flex.Unif where

import PrettyShow

newtype Id = Id (String, Integer)
  deriving (Eq, Ord, Show)

getLabel :: Id -> String
getLabel (Id (label, _)) = label

instance PrettyShow Id where
  prettyShow (Id (str, i)) = "?" <> show i <> "(" <> str <> ")"

data Env = Env {i :: Integer}
  deriving (Show)

nullId :: Id
nullId = Id ("null", -1)

ids :: [Id]
ids = go (-1) where go n = Id ("#" <> show (abs n), n) : go (n - 1)

emptyEnv :: Env
emptyEnv = Env {i = 0}

freshId :: String -> Env -> (Id, Env)
freshId str env =
  ( Id (str, i env),
    env {i = i env + 1}
  )
