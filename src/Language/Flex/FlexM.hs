{-# LANGUAGE FlexibleContexts #-}

module Language.Flex.FlexM where

import Control.Lens
import Control.Monad.Writer (MonadWriter, WriterT)
import qualified Control.Monad.Writer.Class as Writer
import Text.PrettyPrint.HughesPJ
import Prelude hiding (log)

-- | The `FlexM` monad is the base monad under which all the impure comptuations
-- are done in this implementation. In particular, all state is
-- handled by `FlexM`.
type FlexM = WriterT [FlexLog] IO

data FlexLog = FlexLog
  { logLabel :: Doc,
    logBody :: Doc
  }
  deriving (Show)

tell :: MonadWriter [FlexLog] m => FlexLog -> m ()
tell log = Writer.tell [log]
