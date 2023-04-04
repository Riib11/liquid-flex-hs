{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Language.Flex.Refining.Checking where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
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
    _ctxAssertion :: Term
  }

data ScopeItem
  = ScopeForall Crude.TermId
  | ScopeLet Crude.TermId Term

makeLenses ''CheckingCtx

runCheckingM :: CheckingM a -> RefiningM a
runCheckingM m = do
  query <- initQuery
  runReaderT m $
    CheckingCtx
      { _ctxQuery = query,
        _ctxScopeReversed = mempty,
        _ctxAssumptionsReversed = mempty,
        _ctxAssertion = trueTerm -- will be overwritten
      }

ctxScope :: Functor f => ([_] -> f [_]) -> CheckingCtx -> f CheckingCtx
ctxScope = ctxScopeReversed . reversed

ctxAssumptions :: Functor f => ([_] -> f [_]) -> CheckingCtx -> f CheckingCtx
ctxAssumptions = ctxAssumptionsReversed . reversed

introBinding :: Crude.TermId -> Term -> CheckingM a -> CheckingM a
introBinding tmId tm = localExecM do
  -- modify: ctxScopeReversed
  ctxScopeReversed %= (ScopeLet tmId tm :)

  -- modify: ctxQuery._qQuals
  qual <- makeSimpleQualifier (F.symbol tmId)
  (ctxQuery . _qQuals) %= (qual :)

  -- modify: ctxQuery._qEqns
  ex <- lift $ reflTerm tm
  srt <- lift $ reflType $ termType tm
  (ctxQuery . _qEqns)
    %= ( F.Equ
           { eqName = F.symbol tmId,
             eqArgs = [],
             eqBody = ex,
             eqSort = srt,
             eqRec = False
           }
           :
       )

introForall :: Crude.TermId -> CheckingM a -> CheckingM a
introForall tmId = localExecM do
  -- modify: ctxScopeReversed
  ctxScopeReversed %= (ScopeForall tmId :)
  -- modify: ctxQuery._qQuals
  qual <- makeSimpleQualifier (F.symbol tmId)
  (ctxQuery . _qQuals) %= (qual :)

-- introCase :: Term -> Crude.TermId -> [Crude.TermId] -> CheckingM a -> CheckingM a
introCase :: Bool -> Term -> Crude.TypeId -> Crude.TermId -> [Crude.TermId] -> [Type] -> CheckingM a -> CheckingM a
introCase isStruct tm tyId ctorId paramIds paramTypes = localExecM do
  -- modify: ctxScopeReversed
  ctxScopeReversed %= ((ScopeForall <$> paramIds) <>)

  -- modify: ctxQuery._qQuals
  paramQuals <-
    paramIds <&*> \paramId ->
      makeSimpleQualifier (F.symbol paramId)
  (ctxQuery . _qQuals) %= (paramQuals <>)

  ex <- lift $ reflTerm tm

  -- modify: ctxAssumptionsReversed
  -- > assume tm == ctorId [paramIds]
  let propTerm =
        eqTerm
          tm
          ( TermConstructor
              tyId
              ctorId
              isStruct
              (paramIds `zip` paramTypes <&> uncurry TermNamed)
              (TypeNamed tyId)
          )
  propPred <- lift $ reflTerm propTerm
  ctxAssumptionsReversed %= (TermExpr propTerm propPred :)

  -- !TODO figure out how Rewrites work
  -- modify: ctxQuery._qMats
  -- > ?measure (ctorId [paramIds]) = tm
  (ctxQuery . _qMats)
    %= ( F.SMeasure
           { smName = F.symbol @String "?measure", -- !TODO
             smDC = F.symbol ctorId,
             smArgs = F.symbol <$> paramIds,
             smBody = ex
           }
           :
       )

  -- !TODO use this if _qMats doesn't work that way I think
  -- -- modify: _ctxQuery._qCstr
  -- (ctxQuery . _qCstr) %= ...

  return ()

assume :: Term -> CheckingM a -> CheckingM a
assume propTerm = localExecM do
  propPred <- lift $ reflTerm propTerm
  ctxAssumptionsReversed %= (TermExpr propTerm propPred :)

assert :: Doc -> Term -> CheckingM ()
assert sourceDoc tm = flip localExecM checkQuery do
  ex <- lift $ reflTerm tm
  -- modify: ctxAssertion
  ctxAssertion .= tm

  -- assumptions are accounted by as implications in front of the asserted
  -- expression

  -- modify: ctxQuery._qCstr
  asmpExprs <- gets (^.. ctxAssumptions . traverse . to getExpr)
  let ex' = foldr F.PImp ex asmpExprs
  (ctxQuery . _qCstr)
    .= H.Head
      (H.Reft ex')
      (RefiningError $ "unable to prove predicate" <+> ticks (pPrint tm) <+> "arising from" <+> sourceDoc)

-- ** Asserting

checkQuery :: CheckingM ()
checkQuery = do
  asrtTerm <- asks (^. ctxAssertion)
  asmpTerms <- asks (^. ctxAssumptions)
  FlexM.print . vcat $
    [ "checking:",
      nest 4 . vcat $
        [ vcat $ asmpTerms <&> pPrint . getTerm,
          text $ replicate 40 '-',
          pPrint asrtTerm
        ]
    ]
  query <- asks (^. ctxQuery)
  result <- lift $ submitQuery query
  case result of
    (F.Crash errs msg) -> do
      let doc =
            vcat
              [ "checked: crash",
                "refining error:" <+> text msg,
                nest 4 $ vcat (pPrint <$> errs)
              ]
      FlexM.print doc
      throwRefiningError doc
    (F.Unsafe st res) -> do
      let doc =
            vcat
              [ "checked: unsafe",
                "refining errors:",
                nest 4 (vcat $ pPrint <$> res),
                "stats:",
                nest 4 (text $ show st)
              ]
      FlexM.print doc
      throwRefiningError doc
    (F.Safe _st) -> do
      FlexM.print "checked: safe"

-- ** Checking

-- checkTransform ::

-- | Check all refinement constraints that arise within a term.
checkTerm :: Term -> CheckingM ()
checkTerm (TermLiteral _lit _) =
  return ()
checkTerm (TermPrimitive prim ty) =
  checkPrimitive ty prim
checkTerm (TermLet mb_tmId tm1 tm2 _) = do
  checkTerm tm1
  case mb_tmId of
    Nothing -> checkTerm tm2
    Just tmId -> introBinding tmId tm1 $ checkTerm tm2
checkTerm term0@(TermAssert tm1 tm2 _) = do
  assert ("assertion" <+> ticks (pPrint term0)) tm1
  checkTerm tm2
checkTerm (TermNamed _tmId _) =
  return ()
checkTerm (TermApplication _tmId tms _) =
  checkTerm `traverse_` tms
checkTerm (TermConstructor _varntId _ctorId _isVarnt@False tms _) =
  checkTerm `traverse_` tms
checkTerm term0@(TermConstructor structId _ctorId _isStruct@True fields _) = do
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
    (assert ("refined structure construction" <+> ticks (pPrint term0)) structPredTerm)
    (fields `zip` structureFields)
checkTerm (TermMatch tm branches _) = do
  isStruct <- lift $ typeIsStructure (termType tm)
  forM_ branches (uncurry (checkBranch isStruct tm))

checkBranch :: Bool -> Term -> Pattern -> Term -> CheckingM ()
checkBranch isStruct matchTerm (PatternConstructor tyId ctorId ctorParamIds) branchTerm = do
  ctorParamTypes <- lift $ lookupConstructorParameterTypes tyId ctorId
  introCase isStruct matchTerm tyId ctorId ctorParamIds ctorParamTypes $
    checkTerm branchTerm

checkPrimitive :: Type -> Primitive -> CheckingM ()
checkPrimitive _ (PrimitiveTry tm) = checkTerm `traverse_` [tm]
checkPrimitive _ (PrimitiveTuple tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveArray tms) = checkTerm `traverse_` tms
checkPrimitive _ (PrimitiveIf tm1 tm2 tm3) = do
  checkTerm tm1
  -- tm1 == true ==> ...
  assume (eqTerm tm1 trueTerm) $ checkTerm tm2
  -- tm2 == false ==> ...
  assume (eqTerm tm2 falseTerm) $ checkTerm tm3
checkPrimitive _ (PrimitiveAnd tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveOr tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveNot tm) = checkTerm `traverse_` [tm]
checkPrimitive _ (PrimitiveEq tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveAdd tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveExtends tm _tyId) = checkTerm `traverse_` [tm]
