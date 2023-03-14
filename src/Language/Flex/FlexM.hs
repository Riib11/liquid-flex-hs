{-# LANGUAGE FlexibleContexts #-}

module Language.Flex.FlexM where

import Control.Lens
import Control.Monad.Writer (MonadWriter, WriterT (runWriterT), when)
import qualified Control.Monad.Writer.Class as Writer
import Data.Foldable (traverse_)
import Text.PrettyPrint.HughesPJ hiding ((<>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Prelude hiding (log)

-- | The `FlexM` monad is the base monad under which all the impure comptuations
-- are done in this implementation. In particular, all state is
-- handled by `FlexM`.
type FlexM = WriterT [FlexLog] IO

data FlexOptions = FlexOptions
  { flexVerbose :: Bool
  }

defaultFlexOptions :: FlexOptions
defaultFlexOptions =
  FlexOptions
    { flexVerbose = True
    }

runFlexM :: FlexOptions -> FlexM a -> IO a
runFlexM FlexOptions {..} m = do
  (a, logs) <- runWriterT m
  when flexVerbose $ (putStrLn . render . pPrint) `traverse_` logs
  return a

data FlexLog = FlexLog
  { logLabel :: Doc,
    logBody :: Doc
  }
  deriving (Show)

tell :: MonadWriter [FlexLog] m => FlexLog -> m ()
tell log = Writer.tell [log]

instance Pretty FlexLog where
  pPrint FlexLog {..} =
    let str = render $ brackets logLabel
     in text str $$ nest (length str + 1) logBody