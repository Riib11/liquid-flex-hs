{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Language.Flex.FlexM where

import Control.Applicative (liftA)
import Control.Category ((>>>))
import Control.Lens
import Control.Monad
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (local, reader), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState, StateT, evalStateT, gets, modify')
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer (MonadWriter, WriterT (runWriterT), when)
import qualified Control.Monad.Writer.Class as Writer
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Language.Fixpoint.Misc (whenM)
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.PrettyPrint as F
import Language.Flex.Syntax (TermId)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as H
import Text.PrettyPrint.HughesPJ hiding ((<>))
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility (bullet, comps, header, subheader)
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
    flexDebug :: Bool,
    flexSourceFilePath :: FilePath
  }

data FlexEnv = FlexEnv
  { _flexFreshSymbolIndex :: Int,
    _flexTrace' :: [FlexMark],
    _flexStack :: FlexMark
  }

data FlexLog = FlexLog
  { -- | a "path" to where the log's source
    logMark :: FlexMark,
    -- | main content of the log
    logBody :: Doc
  }
  deriving (Show)

newtype FlexMark = FlexMark {unFlexMark :: [FlexMarkStep]}
  deriving newtype (Show, Semigroup, Monoid)

data FlexMarkStep = FlexMarkStep
  { -- | Static indication of where this step arises (e.g. function name, local
    -- definition name)
    flexMarkStepLabel :: Doc,
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

makeLenses ''FlexCtx
makeLenses ''FlexEnv

runFlexM :: FlexCtx -> FlexM a -> IO a
runFlexM ctx@FlexCtx {..} (FlexM m) = do
  env <- initFlexEnv
  (a, logs) <-
    runExceptT (runWriterT (runReaderT (evalStateT m env) ctx)) >>= \case
      Left log -> error $ render $ header "error: runFlexM" $$ pPrint (Dynamic log)
      Right result -> return result
  when flexVerbose $ (putStrLn . render . bullet . pPrint . Static) `traverse_` logs
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
          _flexTrace' = mempty,
          _flexStack = mempty
        }
    )

freshSymbol :: MonadFlex m => String -> m F.Symbol
freshSymbol str = do
  i <- liftFlex $ gets (^. flexFreshSymbolIndex)
  return $ F.symbol (str <> "~" <> show i)

freshenTermId :: MonadFlex m => TermId -> m TermId
freshenTermId = error "freshenTermId"

-- freshSymbol :: (MonadFlex m, F.Symbolic a) => a -> m F.Symbol
-- freshSymbol = freshenSymbol . F.symbol

-- freshenSymbol :: MonadFlex m => F.Symbol -> m F.Symbol
-- freshenSymbol x = do
--   i <- liftFlex $ gets (^. flexFreshSymbolIndex)
--   liftFlex $ modifying flexFreshSymbolIndex (1 +)
--   return $ F.symbol (render (F.pprint x) <> "~" <> show i)

freshInt :: MonadFlex m => m Int
freshInt = do
  i <- liftFlex $ gets (^. flexFreshSymbolIndex)
  liftFlex $ modifying flexFreshSymbolIndex (1 +)
  return i

-- | Implicitly use flexTrace' by reversing it, since it is built up with most
-- recent stacks at the beginning of the trace list
flexTrace :: Lens' FlexEnv [FlexMark]
flexTrace = flexTrace' . (. reverse)

-- | This has to be FlexM because that's the only way to use Reader effect
-- properly.
markSection :: MonadFlex m => [FlexMarkStep] -> m a -> m a
markSection steps m = do
  -- push onto front of stack
  liftFlex $ modify' (flexStack %~ (\(FlexMark steps') -> FlexMark (reverse steps <> steps')))
  mark Static beginStep
  debugMark True beginStep
  -- compute internal result
  a <- m
  -- pop from front of stack
  debugMark True endStep
  mark Static endStep
  liftFlex $ modify' (flexStack %~ (\(FlexMark steps') -> FlexMark (drop (length steps) steps')))
  -- return internal result
  return a
  where
    beginStep = FlexMarkStep "BEGIN" Nothing
    endStep = FlexMarkStep "END" Nothing

markSectionResult ::
  MonadFlex m =>
  FlexMarkStep ->
  (input -> Doc) ->
  input ->
  (output -> Doc) ->
  m output ->
  m output
markSectionResult step pIn input pOut m = do
  -- push onto front of stack
  liftFlex $ modify' (flexStack %~ (\(FlexMark steps') -> FlexMark (step : steps')))
  mark Static beginStep
  debugMark True beginStep
  -- compute internal result
  a <- m
  -- pop from front of stack
  debugMark True (endStep a)
  mark Static (endStep a)
  liftFlex $ modify' (flexStack %~ (\(FlexMark steps') -> FlexMark (drop 1 steps')))
  -- return internal result
  return a
  where
    beginStep = FlexMarkStep "<==  " . Just $ pIn input
    endStep = FlexMarkStep "  ==>" . Just . pOut

mark :: (MonadFlex m, Pretty a) => (FlexMarkStep -> a) -> FlexMarkStep -> m ()
mark xx step = liftFlex do
  -- prepend new stack to trace
  stack' <- gets (^. flexStack . to (FlexMark [step] <>))
  flexTrace %= (stack' :)
  tell $ pPrint $ xx step

-- uses current stack as log mark
tell :: MonadFlex m => Doc -> m ()
tell doc = do
  stack <- liftFlex $ gets (^. flexStack)
  liftFlex $ Writer.tell [FlexLog {logMark = stack, logBody = doc}]

debug :: MonadFlex m => Bool -> Doc -> m ()
debug isActive doc = whenM ((isActive &&) <$> liftFlex (asks flexDebug)) do
  stack <- liftFlex $ gets (^. flexStack)
  liftFlex . liftIO . putStrLn . render . pPrint . Static $
    FlexLog {logMark = stack, logBody = doc}

debugMark :: MonadFlex m => Bool -> FlexMarkStep -> m ()
debugMark isActive = debug isActive . pPrint . Dynamic

-- prints the message and trace
throw :: MonadFlex m => Doc -> m a
throw doc = liftFlex do
  trace <- gets (^. flexTrace)
  stack <- gets (^. flexStack)
  throwError
    FlexLog
      { logMark = stack,
        logBody =
          vcat
            [ header "bug begin",
              doc,
              -- !TODO temporarily disabled, since need to make choices about
              -- how to render logs

              -- subheader "bug stack",
              -- pPrint . Static $ FlexLog stack mempty,
              -- subheader "bug trace",
              -- vcat $ fmap pPrint $ Dynamic . flip FlexLog mempty <$> trace,
              header "bug end"
            ]
      }

-- *** Static FlexLog

newtype Static a = Static {unStatic :: a}

instance Pretty (Static FlexLog) where
  pPrint (Static (FlexLog {..})) =
    -- vcat
    --   [ nest (4 * length (unFlexMark logMark) - 1) $ "●" <> space <> pPrint (Static logMark),
    --     nest (4 + 4 * length (unFlexMark logMark)) logBody
    --   ]
    nest (4 * length (unFlexMark logMark) - 1) $ "●" <+> brackets (pPrint (Static logMark)) <+> logBody

instance Pretty (Static FlexMark) where
  -- pPrint (Static (FlexMark steps)) = hcat $ punctuate (comma <> space) $ pPrint . Static <$> steps
  pPrint (Static (FlexMark steps)) = pPrint . Static $ head steps

instance Pretty (Static FlexMarkStep) where
  pPrint (Static (FlexMarkStep {..})) = flexMarkStepLabel

-- *** Dynamic FlexLog

newtype Dynamic a = Dynamic a

instance Pretty (Dynamic FlexLog) where
  pPrint (Dynamic (FlexLog {..})) =
    vcat
      [ nest (4 * length (unFlexMark logMark) - 1) $ "●" <> space <> pPrint (Dynamic logMark),
        nest (4 * length (unFlexMark logMark)) logBody
      ]

instance Pretty (Dynamic FlexMark) where
  -- pPrint (Dynamic (FlexMark steps)) = vcat $ pPrint . Dynamic <$> steps
  pPrint (Dynamic (FlexMark steps)) = pPrint . Dynamic . head $ steps

instance Pretty (Dynamic FlexMarkStep) where
  pPrint (Dynamic (FlexMarkStep {..})) =
    flexMarkStepLabel
      <+> maybe mempty (\ixDoc -> ":" <+> nest 2 ixDoc) flexmarkStepIndex

debugThing :: (H.Quote m, H.Lift t) => t -> m H.Exp -> m H.Exp -> m H.Exp
debugThing b p x =
  [|
    debugMark
      b
      ( FlexMarkStep
          $(x >>= \x' -> pure $ H.LitE $ H.StringL $ cleanupNameString $ show $ TH.ppr x')
          (Just ($p $x))
      )
    |]

cleanupNameString :: String -> String
cleanupNameString = reverse >>> dropWhile ('_' /=) >>> tail >>> reverse
