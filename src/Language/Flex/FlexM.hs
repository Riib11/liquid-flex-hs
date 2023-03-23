{-# LANGUAGE TemplateHaskell #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Language.Flex.FlexM where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.State (StateT, evalStateT, gets)
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
-- are done in this implementation.
type FlexM = StateT FlexEnv (ReaderT FlexCtx (WriterT [FlexLog] IO))

data FlexCtx = FlexCtx
  { flexVerbose :: Bool,
    sourceFilePath :: FilePath
  }

data FlexEnv = FlexEnv
  { _freshSymbolIndex :: Int
  }

data FlexLog = FlexLog
  { logLabel :: Doc,
    logBody :: Doc
  }
  deriving (Show)

makeLenses ''FlexEnv

runFlexM :: FlexCtx -> FlexM a -> IO a
runFlexM ctx@FlexCtx {..} m = do
  env <- initFlexEnv
  (a, logs) <- runWriterT (runReaderT (evalStateT m env) ctx)
  when flexVerbose $ (putStrLn . render . pPrint) `traverse_` logs
  return a

defaultSourcePos :: MonadReader FlexCtx m => m F.SourcePos
defaultSourcePos = do
  fp <- asks sourceFilePath
  return
    F.SourcePos
      { sourceName = fp,
        sourceLine = F.mkPos 1,
        sourceColumn = F.mkPos 1
      }

defaultLocated :: MonadReader FlexCtx f => a -> f (F.Located a)
defaultLocated a = do
  pos <- defaultSourcePos
  return F.Loc {loc = pos, locE = pos, val = a}

initFlexEnv :: IO FlexEnv
initFlexEnv =
  return
    ( FlexEnv
        { _freshSymbolIndex = 0
        }
    )

freshSymbol :: String -> FlexM F.Symbol
freshSymbol str = do
  i <- gets (^. freshSymbolIndex)
  modifying freshSymbolIndex (1 +)
  return $ F.symbol (str <> "#" <> show i)

tell :: MonadWriter [FlexLog] m => FlexLog -> m ()
tell log = Writer.tell [log]

debug :: MonadIO m => Bool -> FlexLog -> m ()
debug isActive log =
  when isActive . liftIO $
    putStrLn (render . pPrint $ log)

instance Pretty FlexLog where
  pPrint FlexLog {..} =
    let str = render $ brackets logLabel
     in text str $$ nest 2 logBody

pprintInline :: F.PPrint a => a -> Doc
pprintInline =
  text
    . PJ.renderStyle (PJ.style {PJ.mode = PJ.OneLineMode})
    . F.pprint
