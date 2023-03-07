{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Flex.Interpretation where

import Control.Lens hiding (enum)
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Functor
import qualified Data.Map as Map
import Data.Text (Text)
import Flex.Flex
import Flex.Syntax
import PrettyShow
import Utility

-- ** interpretation monad

type Interp = ReaderT Ctx FlexT

runInterp :: Ctx -> Interp a -> FlexT a
runInterp ctx m = runReaderT m ctx

tryInterp :: Interp a -> Interp (Maybe a)
tryInterp m = do
  ctx <- ask
  lift (tryFlexT (runInterp ctx m)) >>= \case
    Left _err -> return Nothing
    Right a -> return $ Just a

-- ** context and environment

data Ctx = Ctx
  { -- | the local term values are in the interpretation context since they must
    -- be fully evaluated before being stored
    _ctxLocals :: Map.Map Text Term,
    -- | which terms are currently being evaluated
    _callStack :: [Term]
  }

emptyCtx :: Ctx
emptyCtx =
  Ctx
    { _ctxLocals = mempty,
      _callStack = []
    }

-- *** lenses

makeLenses ''Ctx

pushCallStack :: Term -> Interp a -> Interp a
pushCallStack tm = locally callStack (tm :)

-- ** processing

-- evaluate all the constants in the module
procModule :: Module -> Interp ()
procModule mdl =
  -- TODO: evaluate imported constants? or they should already be evalauted somehow
  mapM_ procDeclaration (moduleDeclarations mdl)

procDeclaration :: Declaration -> Interp ()
procDeclaration = \case
  DeclarationConstant con_orig -> do
    let x = fromUnqualText $ get_name con_orig
    con <- lift $ lookupConstant x
    constantBody <- evalDefinitionBody (constantBody con)
    envModuleCtx . ctxModuleConstants . ix x .= con {constantBody}
  -- don't need to process anything else
  _ -> return ()

-- *** evaluation

evalDefinitionBody :: DefinitionBody -> Interp DefinitionBody
evalDefinitionBody = \case
  DefinitionBodyTerm tm -> DefinitionBodyTerm <$> evalTerm tm
  DefinitionBodyPrimFun pf -> return $ DefinitionBodyPrimFun pf
  DefinitionBodyPrimConst pc -> return $ DefinitionBodyPrimConst pc
  DefinitionBodyExternal txt -> return $ DefinitionBodyExternal txt
  DefinitionBodyDerived _mb_tm -> unimplemented "derivations"

-- TODO: distinguish between two kinds of failure: interpretation failure (due
-- to malformed input), and runtime failure (can be caught using a `try`)
evalTerm :: Term -> Interp Term
evalTerm tm = do
  tm' <-
    pushCallStack tm $
      case tm ^. termPreterm of
        -- literals are already evaluated
        TermLiteral _lit -> return tm
        TermCast _tm' -> throwInterpError $ "`cast` should not appear in type-checked term: " <> show tm
        TermNamed x -> do
          locs <- asks (^. ctxLocals)
          cs <- asks (^. callStack)
          lift $ lookupTerm id locs (InterpError cs) x
        TermTuple tms -> setPreterm $ TermTuple <$> evalTerm `mapM` tms
        TermArray tms -> setPreterm $ TermArray <$> evalTerm `mapM` tms
        TermBlock (stmts, tm') -> foldr evalStatement (evalTerm tm') stmts
        TermStructure x fields -> setPreterm $ TermStructure x <$> (evalTerm `traverse` fields)
        TermMember tm' txt -> do
          tm' <- evalTerm tm'
          case tm' ^. termPreterm of
            TermStructure x fields ->
              case fields Map.!? txt of
                Nothing -> throwInterpError $ "type error: structure '" <> prettyShow x <> "' doesn't have field '" <> prettyShow txt <> "'"
                Just tm'' -> return tm''
            _ -> throwInterpError "type error: TermMember of non-Structure"
        TermConstructor x mb_tm -> setPreterm $ TermConstructor x <$> (evalTerm `traverse` mb_tm)
        TermApplication x args mb_cxargs -> do
          fun <- lift $ lookupFunction x

          -- evaluate args
          argsList <-
            mconcatMap (\(mb_txt, tm) -> maybe [] (\txt -> [(txt, tm)]) mb_txt)
              <$> traverse
                (bimapM (return . fst) evalTerm)
                ((functionTypeParams . functionType $ fun) `zip` args)
          let argsMap = Map.fromList argsList

          -- all contextual arguments must be given, since already type-checked
          cxargs <- case mb_cxargs of
            Nothing -> throwInterpError "contextual arguments not made explicit"
            Just (Left _) -> throwInterpError "contextual arguments not type-checked"
            Just (Right cxargs) -> return cxargs
          cxargs <- evalTerm `traverse` cxargs

          -- figure out name of corresponding param by matching with
          -- contextual params' types
          let cxparams = functionTypeContextualParams . functionType $ fun
          -- cxargsMap :: Map.Map Text Term
          cxargsMap <-
            sequence $
              Map.mapWithKey
                ( \txt ty ->
                    -- find term in cxargs that has this type; only need to
                    -- check syntactic equality since typchecking normalized
                    -- all the types; don't need to check for ambiguity since
                    -- typechecker handles that
                    maybe
                      (throwInterpError $ "explicit contextual arguments passed to '" <> prettyShow tm <> "', but could not find the contextual argument that has the same type as contextual paramter '" <> prettyShow txt <> ": " <> prettyShow ty <> "'")
                      pure
                      =<< findM
                        ( \tmArg -> case tmArg ^. termMaybeType of
                            Nothing -> throwInterpError $ "explicit contextual argument no type-checked: " <> prettyShow tm
                            Just tyArg -> return (ty == tyArg)
                        )
                        (snd <$> Map.toList cxargs)
                )
                cxparams

          let totalArgsMap = Map.union argsMap cxargsMap

          ffoldr312 -- bind contextual args
            (Map.toList totalArgsMap)
            (\(txt, tmArg) m -> bindTermId txt tmArg m)
            ( evalDefinitionBody (functionBody fun) >>= \case
                DefinitionBodyTerm tm -> evalTerm tm
                DefinitionBodyPrimConst pc -> evalPrimConst pc
                DefinitionBodyPrimFun pf -> do
                  evalPrimFun pf (snd <$> argsList)
                _ -> error "TODO: eval other kinds of DefinitionBody"
            )
        TermIf tmIf tmThen tmElse ->
          evalTerm tmIf
            >>= ( \case
                    TermLiteral (LiteralBit b) ->
                      if b
                        then evalTerm tmThen
                        else evalTerm tmElse
                    _ -> throwInterpError $ "`if` must have a boolean condition term: " <> prettyShow tm
                )
              . (^. termPreterm)
        TermAscribe _tm' _ty -> throwInterpError $ "`ascribe` should not apear in type-checked term: " <> prettyShow tm
        TermMatch _tm' _branches -> unimplemented "pattern matching logic"
  {- TermEq tm1 tm2 ->
    ((==) <$> evalTerm tm1 <*> evalTerm tm2) >>= \case
      True -> setPreterm . return $ TermLiteral (LiteralBit True)
      False -> setPreterm . return $ TermLiteral (LiteralBit False) -}
  debug $ "evalTerm: " <> prettyShow tm <> " ==> " <> prettyShow tm'
  return tm'
  where
    setPreterm pt = tm & setsM termPreterm pt

evalPrimFun :: PrimFun -> [Term] -> Interp Term
evalPrimFun pf args = case (pf, args) of
  (PrimFunEq, [a, b]) -> return $ makeTerm (TermLiteral (LiteralBit (a == b))) TypeBit
  (PrimFunAnd, [a, b])
    | TermLiteral (LiteralBit a) <- a ^. termPreterm,
      TermLiteral (LiteralBit b) <- b ^. termPreterm ->
        return $ makeTerm (TermLiteral (LiteralBit (a && b))) TypeBit
  (PrimFunOr, [a, b])
    | TermLiteral (LiteralBit a) <- a ^. termPreterm,
      TermLiteral (LiteralBit b) <- b ^. termPreterm ->
        return $ makeTerm (TermLiteral (LiteralBit (a || b))) TypeBit
  (PrimFunNot, [a])
    | TermLiteral (LiteralBit a) <- a ^. termPreterm ->
        return $ makeTerm (TermLiteral (LiteralBit (not a))) TypeBit
  _ -> invalid
  where
    invalid = throwInterpError $ "primitive function application was given invalid arguments: " <> prettyShow (TermApplication (idOfPrimFun pf) args Nothing)

evalPrimConst :: PrimConst -> Interp Term
evalPrimConst = error "TODO: evalPrimConst"

evalStatement :: Statement -> Interp a -> Interp a
evalStatement stmt m = do
  debug $ "evalStatement: " <> prettyShow stmt
  case stmt of
    StatementLet pat tm -> do
      tm' <- evalTerm tm
      debug $ "evalStatement: " <> prettyShow stmt <> " ==> " <> prettyShow tm'
      introPattern pat tm' m
    StatementAssert tm ->
      (evalTerm tm <&> (^. termPreterm)) >>= \case
        TermLiteral (LiteralBit True) -> m
        tm' -> throwError . PartialError $ "assertion failure: " <> prettyShow tm <> " ==> " <> prettyShow tm'

-- requires evaluated `tm`
introPattern :: Pattern -> Term -> Interp a -> Interp a
introPattern pat tm = case (pat ^. patternPrepattern, tm ^. termPreterm) of
  (PatternNamed x, _) -> bindTermId x tm
  (PatternDiscard, _) -> id
  (PatternLiteral _, _) -> id

-- requires evaluated `tm`
bindTermId :: Text -> Term -> Interp a -> Interp a
bindTermId x tm = locally ctxLocals (Map.insert x tm)

throwInterpError :: String -> Interp a
throwInterpError str = do
  cs <- asks (^. callStack)
  throwError $ InterpError cs str
