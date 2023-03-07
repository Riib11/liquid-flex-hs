{-# LANGUAGE TemplateHaskell #-}

module Flex.Refining.Translation () where

import Control.Lens
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.State (StateT, gets, modify')
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import Flex.Flex
import Flex.Refining.Syntax (parseSymbol)
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

makeLenses ''TransCtx

type T m a = FlexT (ReaderT TransCtx m) a

-- | transTerm
transTerm :: Monad m => Base.Term -> T m Reft.Term
transTerm tm = case tm ^. Base.termPreterm of
  Base.TermLiteral lit -> transLiteral lit =<< (transType =<< termType tm)
  Base.TermCast _ -> throwError $ RefineError $ "there should be no `cast`s left after type-checking, yet there is: " <> prettyShow tm
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
  Base.TermIf te te' te2 -> error "TODO: transTerm Termif"
  Base.TermAscribe te ty -> throwError $ RefineError $ "there should be no `ascribe`s left after type-checking, yet there is: " <> prettyShow tm
  Base.TermMatch _te _x0 -> error "TODO: transTerm TermMatch"

termType :: Monad m => Base.Term -> T m Base.Type
termType tm = case tm ^. Base.termMaybeType of
  Nothing ->
    throwError . RefineError $
      "expected term to be type-annotated before translation to refinement syntax: " <> prettyShow tm
  Just ty -> pure ty

transBlock :: forall m. Monad m => Base.Block -> T m Reft.Term
transBlock (stmts, tm0) = go stmts []
  where
    go :: [Base.Statement] -> [Reft.Statement] -> T m Reft.Term
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
          x <- freshSymbol ""
          imp <- transTerm imp
          go stmtsBase' (Reft.StatementLet x imp : stmts)
        Base.PatternLiteral lit -> error "TODO: how to translate pattern matching on a literal?"
      Base.StatementAssert tm -> do
        tm <- transTerm tm
        go stmtsBase' (Reft.StatementAssert tm : stmts)

freshSymbol :: Monad m => String -> T m F.Symbol
freshSymbol str = do
  i <- gets (^. envFreshSymbolIndex)
  modify' $ envFreshSymbolIndex %~ (1 +)
  return $ parseSymbol ("$discard" <> show i)

transNameBind :: Monad m => Text -> T m F.Symbol
transNameBind txt = do
  i <- gets (^. envFreshSymbolIndex)
  modify' $ envFreshSymbolIndex %~ (1 +)
  return $ parseSymbol (unpack txt <> "#" <> show i)

transIdBind :: Monad m => Base.Id -> T m F.Symbol
transIdBind = error "TODO: transIdBind"

transIdRef :: Monad m => Base.Id -> T m F.Symbol
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

transType :: Monad m => Base.Type -> T m Reft.BaseType
transType ty0 = case ty0 of
  -- IntSize bounds value
  Base.TypeInt (Base.IntSize s) -> do
    x <- freshSymbol (show s)
    let n = 2 ^ (s - 1) :: Int
    -- -n < x < n
    return $ Reft.TypeAtomic (F.reft x (boundedIntExpr x (-n) n)) Reft.AtomicInt
  Base.TypeUInt (Base.UIntSize s) -> do
    x <- freshSymbol (show s)
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

transLiteral :: Monad m => Base.Literal -> Reft.BaseType -> T m Reft.Term
transLiteral lit ty = pure $ Reft.Term (Reft.TermLiteral lit) ty

transApp :: Monad m => Base.Id -> T m Reft.App
transApp x = Reft.AppVar <$> transIdRef x
