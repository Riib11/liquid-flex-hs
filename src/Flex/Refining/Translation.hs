module Flex.Refining.Translation
  ( transTerm,
    transType,
    transApp,
    transBlock,
    transIdBind,
    transIdRef,
    transNameRef,
    transNameBind,
    transLiteral,
  )
where

import Control.Lens
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT), asks, foldM)
import Control.Monad.State (MonadState (get), StateT, gets, modify')
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import Flex.Flex
import Flex.Refining.Common (Refining, idSymbols)
import qualified Flex.Refining.Syntax as Reft
import qualified Flex.Syntax as Base
import qualified Language.Fixpoint.Types as F
import PrettyShow (PrettyShow (prettyShow))

-- * Translation

--
-- Translate terms and types from Flex to LiquidFlex

-- -- | TransCtx
-- data TransCtx = TransCtx
--   { _idSymbols :: Map.Map Base.Id F.Symbol
--   }

-- makeLenses ''TransCtx

-- -- | Refining
-- type Refining a = ReaderT TransCtx FlexM a

-- runRefining :: Refining a -> FlexM a
-- runRefining m = runReaderT m =<< initTransCtx

-- initTransCtx :: FlexM TransCtx
-- initTransCtx = do
--   env <- get
--   -- need to iterate over functions, constants, and constructors in the current
--   -- module context
--   idSyms <-
--     flip
--       ( foldM . flip $ \fun m -> do
--           s <- freshSymbol (unpack $ Base.functionName fun)
--           return $ Map.insert (Base.fromUnqualName (Base.functionName fun)) s m
--       )
--       (env ^. envModuleCtx . Base.ctxModuleFunctions)
--       =<< flip
--         ( foldM . flip $ \con m -> do
--             let n = Base.constantName con
--             s <- freshSymbol (unpack n)
--             return $ Map.insert (Base.fromUnqualName n) s m
--         )
--         (env ^. envModuleCtx . Base.ctxModuleConstants)
--       =<< flip
--         ( foldM . flip $ \cnstr m -> do
--             let n = Base.constructorName cnstr
--             s <- freshSymbol (unpack n)
--             return $ Map.insert (Base.fromUnqualName n) s m
--         )
--         (env ^. envModuleCtx . Base.ctxModuleConstructors)
--       =<< return Map.empty
--   return
--     TransCtx
--       { _idSymbols = idSyms
--       }

-- | transTerm
transTerm :: Base.Term -> Refining Reft.Term
transTerm tm = case tm ^. Base.termPreterm of
  Base.TermLiteral lit -> transLiteral lit =<< (transType =<< termType tm)
  Base.TermCast _ -> lift $ throwError $ refineError $ "there should be no `cast`s left after type-checking, yet there is: " <> prettyShow tm
  Base.TermNamed x -> transVar x =<< (transType =<< termType tm)
  Base.TermTuple _tes -> error "TODO: transTerm TermTuple"
  Base.TermArray _tes -> error "TODO: transTerm TermArray"
  Base.TermBlock (stmts, tm') -> transBlock (stmts, tm')
  Base.TermStructure _id _map -> error "TODO: transTerm TermStructure"
  Base.TermMember _te _txt -> error "TODO: transTerm TermMember"
  Base.TermConstructor _id _m_te -> error "TODO: transTerm TermConstructor"
  Base.TermApplication x args cxargs -> do
    ty <- transType =<< termType tm
    args <- transTerm `mapM` args
    cxargs <-
      case cxargs of
        Nothing -> return []
        Just (Left _) -> throwError $ refineError $ "expected application with contextual arguments to already be typechecked: " <> prettyShow tm
        Just (Right cxargs') -> transTerm `mapM` Map.elems cxargs'
    app <- transApp x
    pure $ Reft.Term (Reft.TermApp app (args <> cxargs)) ty
  -- TODO: do i need to add TermIf to Reft.Term?
  Base.TermIf _te _te' _te2 -> error "TODO: transTerm Termif"
  Base.TermAscribe _te _ty -> throwError $ refineError $ "there should be no `ascribe`s left after type-checking, yet there is: " <> prettyShow tm
  Base.TermMatch _te _x0 -> error "TODO: transTerm TermMatch"

termType :: Base.Term -> Refining Base.Type
termType tm = case tm ^. Base.termMaybeType of
  Nothing ->
    throwError . refineError $
      "expected term to be type-annotated before translation to refinement syntax: " <> prettyShow tm
  Just ty -> pure ty

transBlock :: forall m. Base.Block -> Refining Reft.Term
transBlock (stmts, tm0) = go stmts []
  where
    go :: [Base.Statement] -> [Reft.Statement] -> Refining Reft.Term
    go [] stmts = do
      ty <- transType =<< termType tm0
      tm0 <- transTerm tm0
      return $ Reft.Term (Reft.TermBlock (reverse stmts, tm0)) ty
    go (stmtsBase : stmtsBase') stmts = case stmtsBase of
      Base.StatementLet pat imp -> case pat ^. Base.patternPrepattern of
        Base.PatternNamed txt -> do
          x <- transNameBind txt
          imp <- transTerm imp
          go stmtsBase' (Reft.StatementLet x imp : stmts)
        Base.PatternDiscard -> do
          x <- lift $ freshSymbol "discard"
          imp <- transTerm imp
          go stmtsBase' (Reft.StatementLet x imp : stmts)
        Base.PatternLiteral lit -> error "TODO: how to translate pattern matching on a literal?"
      Base.StatementAssert tm -> do
        tm <- transTerm tm
        go stmtsBase' (Reft.StatementAssert tm : stmts)

transNameBind :: Text -> Refining F.Symbol
transNameBind txt = do
  i <- gets (^. envFreshSymbolIndex)
  modify' $ envFreshSymbolIndex %~ (1 +)
  return $ parseSymbol (unpack txt <> "#" <> show i)

transNameRef :: Text -> Refining F.Symbol
transNameRef = error "TODO: how exactly should this work?"

transIdBind :: Base.Id -> Refining F.Symbol
transIdBind = error "TODO: transIdBind"

transIdRef :: Base.Id -> Refining F.Symbol
transIdRef x =
  asks (^. idSymbols)
    >>= ( \case
            Nothing ->
              throwError . refineError $
                "transTerm: can't find translation of id '" <> prettyShow x <> "' in environment"
            Just x' -> do
              pure x'
        )
      . Map.lookup x

transType :: Base.Type -> Refining Reft.BaseType
transType ty0 = case ty0 of
  -- IntSize bounds value
  Base.TypeInt (Base.IntSize s) -> do
    x <- lift $ freshSymbol ("TypeInt" <> show s)
    let n = 2 ^ (s - 1) :: Int
    -- -n < x < n
    return $ Reft.TypeAtomic (F.reft x (boundedIntExpr x (-n) n)) Reft.AtomicInt
  Base.TypeUInt (Base.UIntSize s) -> do
    x <- lift $ freshSymbol ("TypeUInt" <> show s)
    let n = 2 ^ s :: Int
    -- 0 <= x < n
    return $ Reft.TypeAtomic (F.reft x (boundedIntExpr x 0 n)) Reft.AtomicInt
  Base.TypeFloat _fs -> error "TODO: transType TypeFloat"
  Base.TypeBit -> return $ Reft.TypeAtomic mempty Reft.AtomicBit
  Base.TypeChar -> return $ Reft.TypeAtomic mempty Reft.AtomicChar
  Base.TypeArray _ty -> error "TODO: transType TypeArray"
  Base.TypeTuple _tys -> error "TODO: transType TypeTuple"
  Base.TypeOptional _ty -> error "TODO: transType TypeOptional"
  Base.TypeStructure _struc -> error "TODO: transType TypeStructure"
  Base.TypeEnumerated _enu -> error "TODO: transType TypeEnumerated"
  Base.TypeVariant _vari -> error "TODO: transType Variant"
  Base.TypeNewtype _new -> error "TODO: transType Newtype"
  Base.TypeNamed _id -> throwError $ refineError $ "TypeNamed should not appear after typechecking since types should be normalized: " <> show ty0
  Base.TypeCast _ty -> throwError $ refineError $ "TypeCast should not appear after typechecking since types should be normalized: " <> show ty0
  Base.TypeUnif _id -> throwError $ refineError $ "TypeUnif should not appear after typechecking since types should be normalized: " <> show ty0
  where
    boundedIntExpr :: F.Symbol -> Int -> Int -> F.Expr
    boundedIntExpr x nMin nMax =
      F.PAnd
        [ F.PAtom F.Le (F.expr nMin) (F.expr x),
          F.PAtom F.Lt (F.expr x) (F.expr nMax)
        ]

transLiteral :: Base.Literal -> Reft.BaseType -> Refining Reft.Term
transLiteral lit ty = pure $ Reft.Term (Reft.TermLiteral lit) ty

transApp :: Base.App -> Refining Reft.App
transApp (Base.AppPrimFun pf) = return $ Reft.AppPrimFun pf
transApp (Base.AppId x) = Reft.AppVar <$> transIdRef x

-- TODO: handle primitive constants differently? what if there is an "error" constant??
transVar :: Base.Id -> Reft.BaseType -> Refining Reft.Term
transVar x ty = Reft.Term <$> (Reft.TermVar <$> transIdRef x) <*> return ty
