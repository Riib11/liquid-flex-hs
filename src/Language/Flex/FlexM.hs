{-# LANGUAGE FlexibleContexts #-}

module Language.Flex.FlexM where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Writer (MonadWriter, WriterT (runWriterT), when)
import qualified Control.Monad.Writer.Class as Writer
import Data.Foldable (traverse_)
import Text.PrettyPrint.HughesPJ hiding ((<>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Prelude hiding (log)

-- | The `FlexM` monad is the base monad under which all the impure comptuations
-- are done in this implementation. In particular, all state is
-- handled by `FlexM`.
type FlexM = ReaderT FlexOptions (WriterT [FlexLog] IO)

-- newtype FlexReaderT r m a = FlexReaderT (ReaderT r m a)
--   deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- class MonadFlexReader

-- instance Functor m => Functor (FlexReaderT r m)

-- instance Monad m => Monad (FlexReaderT r m)

data FlexOptions = FlexOptions
  { flexVerbose :: Bool,
    sourceFilePath :: FilePath
  }

defaultFlexOptions :: FlexOptions
defaultFlexOptions =
  FlexOptions
    { flexVerbose = True,
      sourceFilePath = "<empty>"
    }

runFlexM :: FlexOptions -> FlexM a -> IO a
runFlexM opts@FlexOptions {..} m = do
  (a, logs) <- runWriterT (runReaderT m opts)
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