{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Flex.Flex where

import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Data.Text (Text)
import Flex.Syntax
import qualified Flex.Unif as Unif
import GHC.Generics (Generic)
import qualified Language.Fixpoint.Parse as FP
import qualified Language.Fixpoint.Types as F
import PrettyShow
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
import Utility

type FlexResult a = Either FlexError (a, FlexEnv)

runFlexM :: FlexEnv -> FlexM a -> IO (Either FlexError (a, FlexEnv))
runFlexM env m = runExceptT (runStateT m env)

tryFlexM :: FlexM a -> FlexM (Either FlexError a)
tryFlexM m = do
  env <- get
  (lift . lift) (runFlexM env m) >>= \case
    Left err -> throwError err
    Right (a, env') -> put env' >> return (Right a)

-- TODO: actually, flex should be a monad transformer, and then typing and
-- interpreting should go on the end of the monad transformer, right??
type FlexM = StateT FlexEnv (ExceptT FlexError IO)

-- | FlexError
data FlexError
  = ScopingError String
  | TypingError String
  | InterpError [Term] String
  | RefineError RefineError
  | -- | can be raised by partial functions during interpretation
    PartialError String
  deriving (Show)

instance PrettyShow FlexError where
  prettyShow = \case
    ScopingError str -> "scoping error: " <> str
    TypingError str -> "typing error: " <> str
    InterpError stack str ->
      unlines
        [ "interpretation error: " <> str,
          "call stack:\n" <> unlines (prettyShow <$> stack)
        ]
    RefineError err -> "refining error: " <> prettyShow err
    PartialError str -> "partial error: " <> str

refineError :: String -> FlexError
refineError = RefineError . MakeRefineError

-- | Flex Bug
type Bug = (String, String)

throwBug :: Bug -> FlexM a
throwBug (lbl, msg) =
  error . unlines $
    [ replicate 40 '=',
      "bug: " <> lbl,
      "",
      msg,
      "",
      replicate 40 '='
    ]

fromJust :: Bug -> Maybe a -> FlexM a
fromJust bug = \case
  Nothing -> throwBug bug
  Just a -> return a

-- | RefineError
newtype RefineError = MakeRefineError String
  deriving (Generic, Show)

instance NFData RefineError

instance Exception [RefineError]

instance PrettyShow RefineError where
  prettyShow (MakeRefineError str) = "refinement error: " <> str

messageOfRefineError :: RefineError -> String
messageOfRefineError (MakeRefineError msg) = msg

labelOfRefineError :: RefineError -> Label
labelOfRefineError _ = F.dummySpan

instance F.PPrint RefineError where
  pprintTidy k = F.pprintTidy k . refineErrorFP

instance F.Fixpoint RefineError where
  toFix = PJ.text . messageOfRefineError

instance F.Loc RefineError where
  srcSpan = labelOfRefineError

fpRefineError :: F.Error1 -> RefineError
fpRefineError e = MakeRefineError (show $ F.errMsg e)

refineErrorFP :: RefineError -> F.Error
refineErrorFP err =
  F.err
    (labelOfRefineError err)
    (PJ.text $ messageOfRefineError err)

renderRefineError :: RefineError -> IO PJ.Doc
renderRefineError (MakeRefineError msg) = do
  -- TODO: can also look up snippet where error originated
  return $ PJ.text msg

renderRefineErrors :: [RefineError] -> IO PJ.Doc
renderRefineErrors errs = do
  errs' <- mapM renderRefineError errs
  return $ PJ.vcat (PJ.text "Errors found!" : PJ.text "" : errs')

-- | FlexEnv
data FlexEnv = FlexEnv
  { _envModuleCtx :: ModuleCtx,
    -- | type variable substitution
    _envUnifSubst :: Map.Map Unif.Id Type,
    -- | unification id environment
    _envUnif :: Unif.Env,
    -- | index for fresh symbols used for translation to refinement-checking
    -- syntax
    _envFreshSymbolIndex :: Int
  }

topFlexEnv :: FlexEnv
topFlexEnv =
  FlexEnv
    { _envModuleCtx = topModuleCtx,
      _envUnifSubst = Map.empty,
      _envUnif = Unif.emptyEnv,
      _envFreshSymbolIndex = 0
    }

makeLenses ''FlexEnv

instance PrettyShow FlexEnv where
  prettyShow env =
    unlines . concat $
      [ ["[module context]"],
        ("  " <>) <$> lines (prettyShow (env ^. envModuleCtx)),
        ["[unification substitution]"],
        ("  " <>) . show <$> Map.toList (env ^. envUnifSubst),
        ["[unification id environment]"],
        ["  " <> show (env ^. envUnif)]
      ]

lookupTerm :: (MonadTrans t, Monad (t FlexM)) => (String -> t FlexM a) -> Map.Map Text a -> Id -> (Term -> t FlexM a) -> t FlexM a
lookupTerm e locs x k =
  case tryUnqualify x of
    -- if qualified, try global constants
    Nothing ->
      lift (gets (^. envModuleCtx . ctxModuleConstants . at x)) >>= \case
        Nothing -> e $ "unknown term id: " <> prettyShow x
        Just con -> case constantBody con of
          DefinitionBodyTerm tm -> k tm
          _ -> e $ "looked up the constant but it was not evaluated: " <> prettyShow x
    -- if unqualified, try local vars, then try global constants
    Just txt ->
      case Map.lookup txt locs of
        Just a -> return a
        Nothing ->
          lift (gets (^. envModuleCtx . ctxModuleConstants . at x)) >>= \case
            Nothing -> e $ "unknown term id: " <> prettyShow x
            Just con -> case constantBody con of
              DefinitionBodyTerm tm -> k tm
              _ -> e $ "looked up the constant but it was not evaluated: " <> prettyShow x

lookupFunction :: Id -> FlexM Function
lookupFunction x =
  gets (^. envModuleCtx . ctxModuleFunctions . at x) >>= \case
    Nothing -> throwError . ScopingError $ "unknown function id: " <> prettyShow x
    Just fun -> return fun

lookupConstructor :: Id -> FlexM Constructor
lookupConstructor x =
  gets (^. envModuleCtx . ctxModuleConstructors . at x) >>= \case
    Nothing -> throwError . ScopingError $ "unknown constructor id: " <> prettyShow x
    Just cnstr -> return cnstr

lookupStructure :: Id -> FlexM Structure
lookupStructure x =
  lookupType x >>= \case
    TypeStructure struct -> return struct
    _ -> throwError . TypingError $ "expected to be a structure id: " <> prettyShow x

lookupConstant :: Id -> FlexM Constant
lookupConstant x =
  gets (^. envModuleCtx . ctxModuleConstants . at x) >>= \case
    Nothing -> throwError . ScopingError $ "unknown constant id: " <> prettyShow x
    Just con -> return con

lookupType :: Id -> FlexM Type
lookupType x =
  gets (^. envModuleCtx . ctxModuleTypes . at x) >>= \case
    Nothing -> throwError . ScopingError $ "unknown type id: " <> prettyShow x
    Just (DeclarationTypeStructure struct) -> return $ TypeStructure struct
    Just (DeclarationTypeEnumerated enm) -> return $ TypeEnumerated enm
    Just (DeclarationTypeVariant varnt) -> return $ TypeVariant varnt
    Just (DeclarationTypeNewtype newty) -> return $ TypeNewtype newty
    -- alias's type should already be normalized during typechecking
    Just (DeclarationTypeAlias alias) -> return $ aliasType alias

loadModule :: Module -> FlexM ()
loadModule mdl = do
  -- initialize envModuleCtx for this module
  envModuleCtx .= toOpenedModuleCtx mdl
  -- add imported stuff to module envModuleCtx
  loadImport `mapM_` moduleImports mdl

-- add imported stuff to module envModuleCtx
loadImport :: Import -> FlexM ()
loadImport _imp =
  -- error "TODO"
  return ()

-- ** symbols

freshSymbol :: String -> FlexM F.Symbol
freshSymbol str = do
  i <- gets (^. envFreshSymbolIndex)
  modify' $ envFreshSymbolIndex %~ (1 +)
  return $ parseSymbol (str <> "#" <> show i)

-- | parsing
parseSymbol :: String -> F.Symbol
parseSymbol = FP.doParse' FP.lowerIdP "parseSymbol"

parsePred :: String -> F.Pred
parsePred = FP.doParse' FP.predP "parsePred"

-- ** debugging

-- TODO: better way of writing this constraint?
debug :: (MonadIO (t FlexM)) => String -> t FlexM ()
debug msg =
  when False do
    let ls = lines msg
     in if length ls <= 1
          then liftIO $ putStrLn $ "[>] " <> msg
          else liftIO $ putStrLn $ "[>] " <> head ls <> "\n" <> unlines (indentLines $ drop 1 ls)
