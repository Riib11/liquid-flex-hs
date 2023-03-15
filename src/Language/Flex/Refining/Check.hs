module Language.Flex.Refining.Check where

import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import Language.Flex.Refining.Types

{-}
-- ** Checking

checkTerm :: Term -> Type -> RefiningM Cstr
checkTerm = error "checkTerm"

-- ** Synthesizing

synthTerm :: Term -> RefiningM (Cstr, Type)
synthTerm = undefined

-- ** Subtyping

checkSubtype :: Type -> Type -> RefiningM Cstr
checkSubtype = error "checkSubtype"
-}