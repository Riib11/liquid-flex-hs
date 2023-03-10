{-# LANGUAGE TemplateHaskell #-}

module Language.Flex.FlexM where

import Control.Lens
import Control.Monad.State (StateT)

-- | The `FlexM` monad is the base monad under which all the impure comptuations
-- are done in this implementation. In particular, all state is
-- handled by `FlexM`.
type FlexM a = StateT FlexEnv IO a

-- TODO:
-- - type-unification substitution (for type-checking)
-- - fresh type unification variable id source
data FlexEnv = FlexEnv
  { flexEnvFilePath :: FilePath
  }

makeLenses ''FlexEnv