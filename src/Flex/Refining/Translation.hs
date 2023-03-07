{-# LANGUAGE TemplateHaskell #-}

module Flex.Refining.Translation
  ( TransCtx,
    initTransCtx,
    Translating,
    runTranslating,
    withModuleCtx,
    transTerm,
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
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT), asks)
import Control.Monad.State (StateT, gets, modify')
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import Flex.Flex
import qualified Flex.Refining.Syntax as Reft
import qualified Flex.Syntax as Base
import qualified Language.Fixpoint.Types as F
import PrettyShow (PrettyShow (prettyShow))
import Utility (fromJust')

-- * Translation

--
-- Translate terms and types from Flex to LiquidFlex

-- | TransCtx
data TransCtx = TransCtx
  { _idSymbols :: Map.Map Base.Id F.Symbol
  }

initTransCtx :: TransCtx
initTransCtx =
  TransCtx
    { _idSymbols = error "TODO: initTransCtx.idSymbols: add primitives, or handle in similar way to Typing where primitives are processed specially when gathered from ModuleCtx?"
    }

makeLenses ''TransCtx

-- | Translating
type Translating a = ReaderT TransCtx FlexT a

withModuleCtx :: Base.ModuleCtx -> Translating a -> Translating a
withModuleCtx = error "TODO: withModuleCtx"

runTranslating :: Translating a -> FlexT a
runTranslating m = runReaderT m initTransCtx

-- | transTerm
transTerm :: Base.Term -> Translating Reft.Term
transTerm tm = case tm ^. Base.termPreterm of
  Base.TermLiteral lit -> transLiteral lit =<< (transType =<< termType tm)
  Base.TermCast _ -> lift $ throwError $ RefineError $ "there should be no `cast`s left after type-checking, yet there is: " <> prettyShow tm
  Base.TermNamed x -> Reft.Term <$> (Reft.TermVar <$> transIdRef x) <*> (transType =<< termType tm)
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
        Just (Left _) -> throwError $ RefineError $ "expected application with contextual arguments to already be typechecked: " <> prettyShow tm
        Just (Right cxargs') -> transTerm `mapM` Map.elems cxargs'
    app <- transApp x
    pure $ Reft.Term (Reft.TermApp app (args <> cxargs)) ty
  -- TODO: do i need to add TermIf to Reft.Term?
  Base.TermIf _te _te' _te2 -> error "TODO: transTerm Termif"
  Base.TermAscribe _te _ty -> throwError $ RefineError $ "there should be no `ascribe`s left after type-checking, yet there is: " <> prettyShow tm
  Base.TermMatch _te _x0 -> error "TODO: transTerm TermMatch"

termType :: Base.Term -> Translating Base.Type
termType tm = case tm ^. Base.termMaybeType of
  Nothing ->
    throwError . RefineError $
      "expected term to be type-annotated before translation to refinement syntax: " <> prettyShow tm
  Just ty -> pure ty

transBlock :: forall m. Base.Block -> Translating Reft.Term
transBlock (stmts, tm0) = go stmts []
  where
    go :: [Base.Statement] -> [Reft.Statement] -> Translating Reft.Term
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
          x <- freshSymbol "discard"
          imp <- transTerm imp
          go stmtsBase' (Reft.StatementLet x imp : stmts)
        Base.PatternLiteral lit -> error "TODO: how to translate pattern matching on a literal?"
      Base.StatementAssert tm -> do
        tm <- transTerm tm
        go stmtsBase' (Reft.StatementAssert tm : stmts)

freshSymbol :: String -> Translating F.Symbol
freshSymbol str = do
  i <- gets (^. envFreshSymbolIndex)
  modify' $ envFreshSymbolIndex %~ (1 +)
  return $ Reft.parseSymbol ("$" <> str <> show i)

transNameBind :: Text -> Translating F.Symbol
transNameBind txt = do
  i <- gets (^. envFreshSymbolIndex)
  modify' $ envFreshSymbolIndex %~ (1 +)
  return $ Reft.parseSymbol (unpack txt <> "#" <> show i)

transNameRef :: Text -> Translating F.Symbol
transNameRef = error "TODO: how exactly should this work?"

transIdBind :: Base.Id -> Translating F.Symbol
transIdBind = error "TODO: transIdBind"

transIdRef :: Base.Id -> Translating F.Symbol
transIdRef x =
  asks (^. idSymbols)
    >>= ( \case
            Nothing ->
              throwError . RefineError $
                "transTerm: can't find translation of id '" <> prettyShow x <> "' in environment"
            Just x' -> do
              pure x'
        )
      . Map.lookup x

transType :: Base.Type -> Translating Reft.BaseType
transType ty0 = case ty0 of
  -- IntSize bounds value
  Base.TypeInt (Base.IntSize s) -> do
    x <- freshSymbol ("TypeInt" <> show s)
    let n = 2 ^ (s - 1) :: Int
    -- -n < x < n
    return $ Reft.TypeAtomic (F.reft x (boundedIntExpr x (-n) n)) Reft.AtomicInt
  Base.TypeUInt (Base.UIntSize s) -> do
    x <- freshSymbol ("TypeUInt" <> show s)
    let n = 2 ^ s :: Int
    -- 0 <= x < n
    return $ Reft.TypeAtomic (F.reft x (boundedIntExpr x 0 n)) Reft.AtomicInt
  Base.TypeFloat _fs -> error "TODO: transType TypeFloat"
  Base.TypeBit -> return $ Reft.TypeAtomic mempty Reft.AtomicBit
  Base.TypeChar -> return $ Reft.TypeAtomic mempty Reft.AtomicChar
  Base.TypeArray _ty -> error "TODO: transType TypeArray"
  Base.TypeTuple _tys -> error "TODO: transType TypeTuple"
  Base.TypeOptional _ty -> error "TODO: transType TypeOptional"
  Base.TypeNamed _id -> throwError $ RefineError $ "TypeNamed should not appear after typechecking since types should be normalized: " <> show ty0
  Base.TypeCast _ty -> throwError $ RefineError $ "TypeCast should not appear after typechecking since types should be normalized: " <> show ty0
  Base.TypeUnif _id -> throwError $ RefineError $ "TypeUnif should not appear after typechecking since types should be normalized: " <> show ty0
  Base.TypeStructure _struc -> error "TODO: transType TypeStructure"
  Base.TypeEnumerated _enu -> error "TODO: transType TypeEnumerated"
  Base.TypeVariant _vari -> error "TODO: transType Variant"
  Base.TypeNewtype _new -> error "TODO: transType Newtype"
  where
    boundedIntExpr :: F.Symbol -> Int -> Int -> F.Expr
    boundedIntExpr x nMin nMax =
      F.PAnd
        [ F.PAtom F.Le (F.expr nMin) (F.expr x),
          F.PAtom F.Lt (F.expr x) (F.expr nMax)
        ]

transLiteral :: Base.Literal -> Reft.BaseType -> Translating Reft.Term
transLiteral lit ty = pure $ Reft.Term (Reft.TermLiteral lit) ty

transApp :: Base.Id -> Translating Reft.App
transApp x = Reft.AppVar <$> transIdRef x
