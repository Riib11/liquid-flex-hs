{-# LANGUAGE FlexibleContexts #-}

module Language.Flex.FlexM where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Writer (MonadWriter, WriterT (runWriterT), when)
import qualified Control.Monad.Writer.Class as Writer
import Data.Foldable (traverse_)
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.PrettyPrint as F
import Text.PrettyPrint.HughesPJ hiding ((<>))
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
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

runFlexM :: FlexOptions -> FlexM a -> IO a
runFlexM opts@FlexOptions {..} m = do
  (a, logs) <- runWriterT (runReaderT m opts)
  when flexVerbose $ (putStrLn . render . pPrint) `traverse_` logs
  return a

defaultSourcePos :: MonadReader FlexOptions m => m F.SourcePos
defaultSourcePos = do
  fp <- asks sourceFilePath
  return
    F.SourcePos
      { sourceName = fp,
        sourceLine = F.mkPos 1,
        sourceColumn = F.mkPos 1
      }

defaultLocated :: MonadReader FlexOptions m => a -> m (F.Located a)
defaultLocated a = do
  pos <- defaultSourcePos
  return F.Loc {loc = pos, locE = pos, val = a}

data FlexLog = FlexLog
  { logLabel :: Doc,
    logBody :: Doc
  }
  deriving (Show)

tell :: MonadWriter [FlexLog] m => FlexLog -> m ()
tell log = Writer.tell [log]

debug :: MonadIO m => FlexLog -> m ()
debug log = liftIO $ putStrLn (render . pPrint $ log)

instance Pretty FlexLog where
  pPrint FlexLog {..} =
    let str = render $ brackets logLabel
     in text str $$ nest 2 logBody

pprintInline :: F.PPrint a => a -> Doc
pprintInline =
  text
    . PJ.renderStyle (PJ.style {PJ.mode = PJ.OneLineMode})
    . F.pprint
