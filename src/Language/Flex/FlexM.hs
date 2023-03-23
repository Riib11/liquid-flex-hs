{-# LANGUAGE TemplateHaskell #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Language.Flex.FlexM where

import Control.Applicative (liftA)
import Control.Lens
import Control.Monad
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (local, reader), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState, StateT, evalStateT, gets)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer (MonadWriter, WriterT (runWriterT), when)
import qualified Control.Monad.Writer.Class as Writer
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.PrettyPrint as F
import Text.PrettyPrint.HughesPJ hiding ((<>))
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Prelude hiding (log)

-- | The `FlexM` monad is the base monad under which all the impure comptuations
-- are done in this implementation.
newtype FlexM a = FlexM (StateT FlexEnv (ReaderT FlexCtx (WriterT [FlexLog] (ExceptT FlexLog IO))) a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState FlexEnv,
      MonadReader FlexCtx,
      MonadError FlexLog,
      MonadWriter [FlexLog],
      MonadIO
    )

data FlexCtx = FlexCtx
  { flexVerbose :: Bool,
    flexSourceFilePath :: FilePath,
    _flexStack :: FlexMark
  }

data FlexEnv = FlexEnv
  { _flexFreshSymbolIndex :: Int,
    _flexTrace' :: [FlexMark]
  }

data FlexLog = FlexLog
  { -- | a "path" to where the log's source
    logMark :: FlexMark,
    -- | main content of the log
    logBody :: Doc
  }
  deriving (Show)

newtype FlexMark = FlexMark [FlexMarkStep]
  deriving newtype (Show, Semigroup, Monoid)

data FlexMarkStep = FlexMarkStep
  { -- | Static indication of where this step arises (e.g. function name, local
    -- definition name)
    flexMarkStepLabel :: String,
    -- | Dynamic index of relevant runtime values at this step (e.g. arguments
    -- to function, value of local definition)
    flexmarkStepIndex :: Maybe Doc
  }
  deriving (Show)

-- | Apparently I can't use Monad as superclass here, since then in the instance
-- for `MonadFlex' (t m)` it requires as a premise `Monad (t m)` which fails
-- termination checking since `t m` is as big as the output constrained type.
class MonadFlex' m where
  liftFlex :: FlexM a -> m a

instance (Monad m, MonadTrans t, MonadFlex' m) => MonadFlex' (t m) where
  liftFlex = lift . liftFlex

instance MonadFlex' FlexM where
  liftFlex = id

type MonadFlex m = (Monad m, MonadFlex' m)

-- instance MonadFlex FlexM

-- liftAFlex :: MonadFlex m => (FlexM a -> FlexM b) -> m a -> m b
-- liftAFlex k ma = liftA k ma

-- liftFlex . k . return =<< ma

makeLenses ''FlexCtx
makeLenses ''FlexEnv

runFlexM :: FlexCtx -> FlexM a -> IO a
runFlexM ctx@FlexCtx {..} (FlexM m) = do
  env <- initFlexEnv
  (a, logs) <-
    runExceptT (runWriterT (runReaderT (evalStateT m env) ctx)) >>= \case
      Left log -> error $ render $ "[error: runFlexM]" $$ pPrint (Dynamic log)
      Right result -> return result
  when flexVerbose $ (putStrLn . render . ("● " <+>) . pPrint . Static) `traverse_` logs
  return a

defaultSourcePos :: MonadFlex m => m F.SourcePos
defaultSourcePos = do
  fp <- liftFlex $ asks flexSourceFilePath
  return
    F.SourcePos
      { sourceName = fp,
        sourceLine = F.mkPos 1,
        sourceColumn = F.mkPos 1
      }

defaultLocated :: MonadFlex m => a -> m (F.Located a)
defaultLocated a = do
  pos <- defaultSourcePos
  return F.Loc {loc = pos, locE = pos, val = a}

initFlexEnv :: IO FlexEnv
initFlexEnv =
  return
    ( FlexEnv
        { _flexFreshSymbolIndex = 0,
          _flexTrace' = []
        }
    )

freshSymbol :: MonadFlex m => String -> m F.Symbol
freshSymbol str = do
  i <- liftFlex $ gets (^. flexFreshSymbolIndex)
  liftFlex $ modifying flexFreshSymbolIndex (1 +)
  return $ F.symbol (str <> "#" <> show i)

-- | Implicitly use flexTrace' by reversing it, since it is built up with most
-- recent stacks at the beginning of the trace list
flexTrace :: Lens' FlexEnv [FlexMark]
flexTrace = flexTrace' . (. reverse)

-- | This has to be FlexM because that's the only way to use Reader effect
-- properly.
markSection :: [FlexMarkStep] -> FlexM a -> FlexM a
markSection steps =
  local (flexStack %~ (\(FlexMark steps') -> FlexMark (steps' <> steps)))

mark :: MonadFlex m => [FlexMarkStep] -> m ()
mark steps = liftFlex do
  -- prepend new stack to trace
  FlexMark steps' <- asks (^. flexStack)
  let stack' = FlexMark $ steps' <> steps
  flexTrace %= (stack' :)
  tell . pPrint . Dynamic $ stack'

-- uses current stack as log mark
tell :: MonadFlex m => Doc -> m ()
tell doc = do
  stack <- liftFlex $ asks (^. flexStack)
  liftFlex $ Writer.tell [FlexLog {logMark = stack, logBody = doc}]

debug :: MonadFlex m => Bool -> Doc -> m ()
debug isActive doc = do
  stack <- liftFlex $ asks (^. flexStack)
  when isActive . liftFlex . liftIO . putStrLn . render . ("● " <+>) . pPrint . Static $
    FlexLog {logMark = stack, logBody = doc}

debugMark :: MonadFlex m => Bool -> FlexMarkStep -> m ()
debugMark isActive = debug isActive . pPrint . Dynamic

-- prints the message and trace
throw :: MonadFlex m => Doc -> m a
throw doc = liftFlex do
  trace <- gets (^. flexTrace)
  stack <- asks (^. flexStack)
  throwError
    FlexLog
      { logMark = stack,
        logBody =
          vcat
            [ "[ bug begin ]" <> text (replicate 40 '='),
              doc,
              "[ bug stack ]" <> text (replicate 40 '-'),
              pPrint . Static $ stack,
              "[ bug trace ]" <> text (replicate 40 '-'),
              vcat $ fmap ("[>]" <+>) $ pPrint . Dynamic <$> trace,
              "[ bug end ]"
            ]
      }

-- *** Static FlexLog

newtype Static a = Static a

instance Pretty (Static FlexLog) where
  pPrint (Static (FlexLog {..})) = pPrint (Static logMark) $$ nest 2 logBody

instance Pretty (Static FlexMark) where
  pPrint (Static (FlexMark steps)) = brackets $ vcat $ punctuate "." $ pPrint . Static <$> steps

instance Pretty (Static FlexMarkStep) where
  pPrint (Static (FlexMarkStep {..})) = text flexMarkStepLabel

-- *** Dynamic FlexLog

newtype Dynamic a = Dynamic a

instance Pretty (Dynamic FlexLog) where
  pPrint (Dynamic (FlexLog {..})) = pPrint (Dynamic logMark) $$ nest 2 logBody

instance Pretty (Dynamic FlexMark) where
  pPrint (Dynamic (FlexMark steps)) =
    case steps of
      [] -> mempty
      steps' -> pPrint $ Dynamic (last steps')

instance Pretty (Dynamic FlexMarkStep) where
  pPrint (Dynamic (FlexMarkStep {..})) =
    text flexMarkStepLabel
      <+> ":"
      <+> nest 2 (fromMaybe mempty flexmarkStepIndex)
