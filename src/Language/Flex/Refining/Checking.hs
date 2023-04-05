{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Language.Flex.Refining.Checking where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable hiding (toList)
import Data.HashMap.Strict (toList)
import Data.Maybe
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Types as F
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Constraint
import Language.Flex.Refining.Query
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Reflecting
import Language.Flex.Refining.Syntax
import Language.Flex.Refining.Translating
import qualified Language.Flex.Syntax as Crude
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility hiding (for)

-- * Checking

-- Main forms where refinement-checking comes up:
-- - structure construction
-- - pattern matching
-- - if-then-else
-- - transform call (need to assume refinements of refined output types)
-- - refined primitives
-- - assert
-- - let (quantify)

-- ** CheckingM

type CheckingM a = ReaderT CheckingCtx RefiningM a

data CheckingCtx = CheckingCtx
  { _ctxQuery :: Query,
    _ctxScopeReversed :: [ScopeItem],
    _ctxAssumptionsReversed :: [TermExpr],
    _ctxAssertion :: TermExpr
  }

instance Pretty CheckingCtx where
  pPrint CheckingCtx {..} =
    vcat
      [ "query:",
        nest 2 . vcat $
          [ "qQuals:",
            nest 2 . vcat $ F.pprint <$> _ctxQuery ^. _qQuals,
            "qCon:",
            nest 2 $ vcat $ (\(x, srt) -> F.pprint x <+> ":" <+> F.pprint srt) <$> _ctxQuery ^. _qCon . to toList,
            "qEqns:",
            nest 2 . vcat $ F.pprint <$> _ctxQuery ^. _qEqns,
            "qMats:",
            nest 2 . vcat $ F.pprint <$> _ctxQuery ^. _qMats,
            "qData:",
            nest 2 . vcat $ F.pprint <$> _ctxQuery ^. _qData
          ],
        "scope:",
        nest 2 . vcat $ pPrint <$> reverse _ctxScopeReversed,
        "assumptions:",
        nest 2 . vcat $ pPrint <$> reverse _ctxAssumptionsReversed,
        "assertion:",
        nest 2 $ pPrint _ctxAssertion
      ]

data ScopeItem
  = ScopeForall Crude.TermId TypeSort
  | ScopeLet Crude.TermId TermExpr TypeSort
  deriving (Show)

instance Pretty ScopeItem where
  pPrint (ScopeForall ti tysrt) = "forall" <+> pPrint ti <+> ":" <+> pPrint tysrt
  pPrint (ScopeLet ti tmex tysrt) = "let" <+> pPrint ti <+> "=" <+> pPrint tmex <+> ":" <+> pPrint tysrt

makeLenses ''CheckingCtx

runCheckingM :: CheckingM a -> RefiningM a
runCheckingM m = do
  query <- initQuery
  runReaderT m $
    CheckingCtx
      { _ctxQuery = query,
        _ctxScopeReversed = mempty,
        _ctxAssumptionsReversed = mempty,
        _ctxAssertion = TermExpr trueTerm (F.prop True) -- will be overwritten
      }

ctxScope :: Functor f => ([_] -> f [_]) -> CheckingCtx -> f CheckingCtx
ctxScope = ctxScopeReversed . reversed

ctxAssumptions :: Functor f => ([_] -> f [_]) -> CheckingCtx -> f CheckingCtx
ctxAssumptions = ctxAssumptionsReversed . reversed

introBinding :: Crude.TermId -> Term -> CheckingM a -> CheckingM a
introBinding tmId tm = localExecM do
  ex <- lift $ reflTerm tm
  srt <- lift $ reflType $ termType tm

  -- modify: ctxScopeReversed
  ctxScopeReversed %= (ScopeLet tmId (TermExpr tm ex) (TypeSort (termType tm) srt) :)

introForall :: Crude.TermId -> TypeSort -> CheckingM a -> CheckingM a
introForall tmId tysrt = localExecM do
  -- modify: ctxScopeReversed
  ctxScopeReversed %= (ScopeForall tmId tysrt :)

  -- TODO: intro all assumptions about subterms, wherever we have refined types
  return ()

introCase :: Term -> Crude.TypeId -> Crude.TermId -> [Crude.TermId] -> [Type] -> CheckingM a -> CheckingM a
introCase tm tyId ctorId paramIds paramTypes = localExecM do
  paramTypeSorts <-
    lift $
      paramTypes <&*> \ty ->
        TypeSort ty <$> reflType ty

  -- modify: ctxScopeReversed
  ctxScopeReversed %= ((uncurry ScopeForall <$> (paramIds `zip` paramTypeSorts)) <>)

  -- modify: ctxAssumptionsReversed
  -- > assume tm == ctorId [paramIds]
  let propTerm =
        eqTerm
          tm
          ( TermConstructor
              tyId
              ctorId
              (paramIds `zip` paramTypes <&> uncurry TermNamed)
              (TypeNamed tyId)
          )
  propPred <- lift $ reflTerm propTerm
  ctxAssumptionsReversed %= (TermExpr propTerm propPred :)

  return ()

assume :: Term -> CheckingM a -> CheckingM a
assume propTerm = localExecM do
  propPred <- lift $ reflTerm propTerm
  ctxAssumptionsReversed %= (TermExpr propTerm propPred :)

assert :: Doc -> Term -> CheckingM ()
assert sourceDoc tm = flip localExecM checkQuery do
  ex0 <- lift $ reflTerm tm
  -- modify: ctxAssertion
  ctxAssertion .= TermExpr tm ex0

  -- True: use implications
  -- False: use witnesses
  cstr0 <-
    if False
      then do
        -- wrap in assumptions using F.PImp
        asmpExprs <- gets (^.. ctxAssumptions . traverse . to getExpr)
        let ex1 = foldr F.PImp ex0 asmpExprs
        FlexM.debug True $ "ex' =" <+> F.pprint ex1

        return $
          H.Head
            (H.Reft ex1)
            (RefiningError $ "unable to prove predicate" <+> ticks (pPrint tm) <+> "arising from" <+> sourceDoc)
      else do
        -- !TODO this _actually_ works!
        -- wrap in assumptions using existentially-quantified witnesses
        let cstr =
              H.Head
                (H.Reft ex0)
                (RefiningError $ "unable to prove predicate" <+> ticks (pPrint tm) <+> "arising from" <+> sourceDoc)
        asmps <- gets (^. ctxAssumptions)
        let cstr' =
              foldr
                ( \tmex ->
                    H.Any
                      H.Bind
                        { bSym = F.symbol . render $ "wittness to assumption:" <+> ticks (pPrint tmex),
                          bSort = F.boolSort,
                          bPred = H.Reft $ getExpr tmex,
                          bMeta = RefiningError $ "assumption:" <+> ticks (pPrint tmex)
                        }
                )
                cstr
                asmps
        return cstr'

  -- wrap in scope
  scope <- gets (^. ctxScope)
  let cstr1 =
        foldr
          ( \case
              (ScopeForall tmId tysrt) ->
                H.All
                  H.Bind
                    { bSym = makeTermIdSymbol tmId,
                      bSort = getSort tysrt,
                      bPred = H.Reft (F.prop True),
                      bMeta = RefiningError $ "intro" <+> pPrint tmId <+> ":" <+> pPrint tysrt
                    }
              (ScopeLet tmId tmex tysrt) ->
                H.All
                  H.Bind
                    { bSym = makeTermIdSymbol tmId,
                      bSort = getSort tysrt,
                      bPred = H.Reft $ F.PAtom F.Eq (F.eVar (makeTermIdSymbol tmId)) (getExpr tmex),
                      bMeta = RefiningError $ "intro" <+> pPrint tmId <+> ":" <+> pPrint tysrt
                    }
          )
          cstr0
          scope

  -- modify: ctxQuery._qCstr
  (ctxQuery . _qCstr) .= cstr1

-- ** Check Query

checkQuery :: CheckingM ()
checkQuery = do
  ctx <- ask
  FlexM.print . vcat $ ["checking:", nest 2 $ pPrint ctx]
  query <- asks (^. ctxQuery)
  result <- lift $ submitQuery query
  case result of
    (F.Crash errs msg) -> do
      let doc =
            vcat
              [ "checked: crash",
                "refining error:" <+> text msg,
                nest 2 $
                  vcat
                    ( errs <&> \(err, mb_str) ->
                        vcat
                          [ pPrint err,
                            maybe mempty (vcat . fmap text . lines) mb_str
                          ]
                    ),
                "refinement checking context:",
                nest 2 $ pPrint ctx
              ]
      FlexM.print doc
      throwRefiningError doc
    (F.Unsafe st res) -> do
      let doc =
            vcat
              [ "checked: unsafe",
                "refining errors:",
                nest 2 (vcat $ pPrint <$> res),
                "stats:",
                nest 2 (text $ show st)
              ]
      FlexM.print doc
      throwRefiningError doc
    (F.Safe _st) -> do
      FlexM.print "checked: safe"

-- ** Checking

checkTransform :: Function -> CheckingM ()
checkTransform Function {..} = FlexM.markSection [FlexM.FlexMarkStep ("checkTransform:" <+> pPrint functionId) Nothing] do
  -- introduce parameters
  body <- lift $ transTerm functionBody
  paramIdTypeSorts <- lift $ functionParameters <&*> \(tmId, ty) -> (tmId,) . TypeSort ty <$> reflType ty
  FlexM.debug True $ "checkTransform: body =" <+> pPrint body
  foldr
    (uncurry introForall)
    -- check body
    (checkTerm body)
    paramIdTypeSorts

checkConstant :: Crude.Term Type -> CheckingM ()
checkConstant tm = do
  tm' <- lift $ transTerm tm
  checkTerm tm'

-- | Check all refinement constraints that arise within a term.
checkTerm :: Term -> CheckingM ()
checkTerm term =
  FlexM.markSection [FlexM.FlexMarkStep ("checkTerm:" <+> pPrintShallowTerm term) Nothing] $
    checkTerm' term

checkTerm' :: Term -> CheckingM ()
checkTerm' (TermLiteral _lit _) =
  return ()
checkTerm' (TermPrimitive prim ty) =
  checkPrimitive ty prim
checkTerm' (TermLet mb_tmId tm1 tm2 _) = do
  checkTerm tm1
  case mb_tmId of
    Nothing -> checkTerm tm2
    Just tmId -> introBinding tmId tm1 $ checkTerm tm2
checkTerm' term0@(TermAssert tm1 tm2 _) = do
  assert ("assertion" <+> ticks (pPrintShallowTerm term0)) tm1
  checkTerm tm2
checkTerm' (TermNamed _tmId _) =
  return ()
checkTerm' (TermApplication _tmId tms _) =
  checkTerm `traverse_` tms
checkTerm' term0@(TermStructure structId fields _ty) = do
  -- check fields
  checkTerm `traverse_` fields

  -- check that fields satisfy the structure's refinement; for each field, intro
  -- binding, then assert structure predicate
  Structure {..} <- lift $ lookupStructure structId
  structPredTerm <- lift $ transRefinement structureRefinement
  foldr
    ( \(fieldTerm, (fieldId, _)) ->
        introBinding (Crude.fromFieldIdToTermId fieldId) fieldTerm
    )
    (assert ("refined structure construction" <+> ticks (pPrintShallowTerm term0)) structPredTerm)
    (fields `zip` structureFields)
checkTerm' (TermMember _structId tm _fieldId _ty) = do
  checkTerm tm
checkTerm' (TermConstructor _varntId _ctorId tms _) = do
  checkTerm `traverse_` tms
checkTerm' (TermMatch tm branches _) = do
  forM_ branches (uncurry (checkBranch tm))

checkBranch :: Term -> Pattern -> Term -> CheckingM ()
checkBranch matchTerm (PatternConstructor tyId ctorId ctorParamIds) branchTerm = do
  ctorParamTypes <- lift $ lookupConstructorParameterTypes tyId ctorId
  introCase matchTerm tyId ctorId ctorParamIds ctorParamTypes $
    checkTerm branchTerm

checkPrimitive :: Type -> Primitive -> CheckingM ()
checkPrimitive _ (PrimitiveTry tm) = checkTerm `traverse_` [tm]
checkPrimitive _ (PrimitiveTuple tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveArray tms) = checkTerm `traverse_` tms
checkPrimitive _ (PrimitiveIf tm1 tm2 tm3) = do
  checkTerm tm1
  -- tm1 == true ==> ...
  assume (eqTerm tm1 trueTerm) $ checkTerm tm2
  -- tm1 == false ==> ...
  assume (eqTerm tm1 falseTerm) $ checkTerm tm3
checkPrimitive _ (PrimitiveAnd tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveOr tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveNot tm) = checkTerm `traverse_` [tm]
checkPrimitive _ (PrimitiveEq tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveAdd tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveExtends tm _tyId) = checkTerm `traverse_` [tm]
