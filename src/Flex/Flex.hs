{-# LANGUAGE TemplateHaskell #-}

module Flex.Flex where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Data.Text (Text)
import Flex.Syntax
import qualified Flex.Unif as Unif
import PrettyShow
import Utility

type FlexResult a = Either FlexError (a, FlexEnv)

runFlexT :: FlexEnv -> FlexT a -> IO (Either FlexError (a, FlexEnv))
runFlexT env m = runExceptT (runStateT m env)

tryFlexT :: FlexT a -> FlexT (Either FlexError a)
tryFlexT m = do
  env <- get
  (lift . lift) (runFlexT env m) >>= \case
    Left err -> throwError err
    Right (a, env') -> put env' >> return (Right a)

-- TODO: actually, flex should be a monad transformer, and then typing and
-- interpreting should go on the end of the monad transformer, right??
type FlexT = StateT FlexEnv (ExceptT FlexError IO)

data FlexError
  = ScopingError String
  | TypingError String
  | InterpError [Term] String
  | RefineError String
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
    RefineError str -> "refining error: " <> str
    PartialError str -> "partial error: " <> str

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

lookupTerm :: (Term -> a) -> Map.Map Text a -> (String -> FlexError) -> Id -> FlexT a
lookupTerm proj locs makeError x =
  case tryUnqualify x of
    -- if qualified, try global constants
    Nothing ->
      gets (^. envModuleCtx . ctxModuleConstants . at x) >>= \case
        Nothing -> throwError . ScopingError $ "unknown term id: " <> prettyShow x
        Just con -> case constantBody con of
          DefinitionBodyTerm tm -> return (proj tm)
          _ -> throwError . makeError $ "looked up the constant but it was not evaluated: " <> prettyShow x
    -- if unqualified, try local vars, then try global constants
    Just txt ->
      case Map.lookup txt locs of
        Just a -> return a
        Nothing ->
          gets (^. envModuleCtx . ctxModuleConstants . at x) >>= \case
            Nothing -> throwError . ScopingError $ "unknown term id: " <> prettyShow x
            Just con -> case constantBody con of
              DefinitionBodyTerm tm -> return (proj tm)
              _ -> throwError . makeError $ "looked up the constant but it was not evaluated: " <> prettyShow x

lookupFunction :: Id -> FlexT Function
lookupFunction x =
  gets (^. envModuleCtx . ctxModuleFunctions . at x) >>= \case
    Nothing -> throwError . ScopingError $ "unknown function id: " <> prettyShow x
    Just fun -> return fun

lookupConstructor :: Id -> FlexT Constructor
lookupConstructor x =
  gets (^. envModuleCtx . ctxModuleConstructors . at x) >>= \case
    Nothing -> throwError . ScopingError $ "unknown constructor id: " <> prettyShow x
    Just cnstr -> return cnstr

lookupStructure :: Id -> FlexT Structure
lookupStructure x =
  lookupType x >>= \case
    TypeStructure struct -> return struct
    _ -> throwError . TypingError $ "expected to be a structure id: " <> prettyShow x

lookupConstant :: Id -> FlexT Constant
lookupConstant x =
  gets (^. envModuleCtx . ctxModuleConstants . at x) >>= \case
    Nothing -> throwError . ScopingError $ "unknown constant id: " <> prettyShow x
    Just con -> return con

lookupType :: Id -> FlexT Type
lookupType x =
  gets (^. envModuleCtx . ctxModuleTypes . at x) >>= \case
    Nothing -> throwError . ScopingError $ "unknown type id: " <> prettyShow x
    Just (DeclarationTypeStructure struct) -> return $ TypeStructure struct
    Just (DeclarationTypeEnumerated enm) -> return $ TypeEnumerated enm
    Just (DeclarationTypeVariant varnt) -> return $ TypeVariant varnt
    Just (DeclarationTypeNewtype newty) -> return $ TypeNewtype newty
    -- alias's type should already be normalized during typechecking
    Just (DeclarationTypeAlias alias) -> return $ aliasType alias

loadModule :: Module -> FlexT ()
loadModule mdl = do
  -- initialize envModuleCtx for this module
  envModuleCtx .= toOpenedModuleCtx mdl
  -- add imported stuff to module envModuleCtx
  loadImport `mapM_` moduleImports mdl

-- add imported stuff to module envModuleCtx
loadImport :: Import -> FlexT ()
loadImport _imp =
  -- error "TODO"
  return ()

-- ** debugging

-- TODO: better way of writing this constraint?
debug :: (MonadIO (t FlexT)) => String -> t FlexT ()
debug msg =
  when False do
    let ls = lines msg
     in if length ls <= 1
          then liftIO $ putStrLn $ "[>] " <> msg
          else liftIO $ putStrLn $ "[>] " <> head ls <> "\n" <> unlines (indentLines $ drop 1 ls)
