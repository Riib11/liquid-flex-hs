{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-# HLINT ignore "Redundant flip" #-}

module Language.Flex.Refining.Checking where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable hiding (toList)
import Data.HashMap.Strict (toList)
import qualified Data.Map as Map
import Data.Maybe
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Types as F
import Language.Flex.FlexM (freshenTermId)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Constraint
import Language.Flex.Refining.Primitive (primitiveDataDecls)
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
    -- _ctxStructureProperties :: [H.Bind RefiningError],
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
  ctx <- initCheckingContext
  runReaderT m ctx

ctxScope :: Functor f => ([_] -> f [_]) -> CheckingCtx -> f CheckingCtx
ctxScope = ctxScopeReversed . reversed

ctxAssumptions :: Functor f => ([_] -> f [_]) -> CheckingCtx -> f CheckingCtx
ctxAssumptions = ctxAssumptionsReversed . reversed

introBinding :: Crude.TermId -> Term -> CheckingM a -> CheckingM a
introBinding tmId tm = localExecM do
  ex <- lift $ reflTerm tm
  srt <- lift $ reflType $ termType tm
  ctxScopeReversed %= (ScopeLet tmId (TermExpr tm ex) (TypeSort (termType tm) srt) :)

introForall :: Crude.TermId -> TypeSort -> CheckingM a -> CheckingM a
introForall tmId tysrt = localExecM do
  ctxScopeReversed %= (ScopeForall tmId tysrt :)

-- !TODO intro all assumptions about subterms, wherever we have refined types

-- !TODO this it not the right way since there can be recursive types, and in
-- principle its wrong also -- shoud be propogating refinements with refinement types rather htan

-- -- according to the term's type, assume all refinements on its type of
-- -- nested fields' types
-- assumedTypeRefinements :: Term -> TypeSort -> CheckingM a -> CheckingM a
-- assumedTypeRefinements tm tysrt m = do
--   ex <- lift $ reflTerm tm
--   case getType tysrt of
--     (TypeArray ty) -> _wt -- !TODO assume refinement on each element of array
--     (TypeTuple ty1 ty2) ->
--       flip comps m $
--         [ assumedTypeRefinements (TermMember _ _ _ _) _
--         ]
--     (TypeOptional ty') -> _wv
--     (TypeNamed ti) -> _ww
--     _ -> m

introCase :: Term -> Crude.TypeId -> Crude.TermId -> [Crude.TermId] -> [Type] -> CheckingM a -> CheckingM a
introCase tm tyId ctorId paramIds paramTypes = localExecM do
  paramTypeSorts <-
    lift $
      paramTypes <&*> \ty ->
        TypeSort ty <$> reflType ty

  ctxScopeReversed %= ((uncurry ScopeForall <$> (paramIds `zip` paramTypeSorts)) <>)

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

  (ctxQuery . _qCstr) .= cstr1

-- ** Initialize Context

initCheckingContext :: RefiningM CheckingCtx
initCheckingContext = do
  let query =
        H.Query
          { qQuals = mempty,
            qVars = mempty,
            qCstr = H.CAnd [], -- will be overwritten during checking
            qCon = mempty,
            qDis = mempty,
            qEqns = mempty,
            qMats = mempty,
            qData = mempty
          }
  let ctx =
        CheckingCtx
          { _ctxQuery = query,
            _ctxScopeReversed = mempty,
            _ctxAssumptionsReversed = mempty,
            _ctxAssertion = TermExpr trueTerm (F.prop True) -- will be overwritten
          }
  flip execStateT ctx $ do
    -- intro datatypes
    -- - intro primitive datatypes
    -- - intro user datatypes
    ctxQuery . _qData %= (primitiveDataDecls <>)
    introUserDatatypes

    -- intro transforms (as uninterpreted functions)
    introTransforms

    -- intro constants (as equations)
    introConstants

    return ()

introUserDatatypes :: StateT CheckingCtx RefiningM ()
introUserDatatypes = do
  -- intro structures
  structs <- asks (^. ctxStructures)
  forM_ (Map.elems structs) \struct@Structure {..} -> do
    dd <- lift $ reflStructure struct
    ctxQuery . _qData %= (dd :)

  -- intro refinement properties
  {-
    struct A {
      b: B;
      assert P(b);
    }

    struct B {
      y: A;
      assert Q(y);
    }

    predicate isA(a : A)
    axiom forall a . isA(a) ==>
      P(a.b) && -- refinement of A
      isB(a.b)  -- refinement of B (inherited)

    predicate isB(b : B)
    axiom forall b . isB(b) ==>
      P(b.a) && -- refinement of B
      isA(b.a)  -- refinement of A (inherited)
  -}

  {- !TODO need to implement substTerm for Refining terms

      structSort <- lift $ reflType (TypeNamed structureId)
      let structPropSymbol = makeStructurePropertySymbol structureId

      -- predicate is<Struct>(s : <Struct>)
      (ctxQuery . _qCon . at structPropSymbol) ?= F.FFunc structSort F.boolSort

      -- is<Struct>(instance) = P[x := instance.x, y := instance.y]

      -- let structPropTerm =
      --       Crude.substTerm
      --         ( Map.fromList $
      --             structureFields <&> \(fieldId, fieldType) ->
      --               ( Crude.fromFieldIdToTermId fieldId,
      --                 Crude.TermMember (Crude.TermNeutral (Crude.Neutral (Crude.TermId "instance")) (TypeNamed structureId)) fieldId fieldType
      --               )
      --         )
      --         (Crude.unRefinement structureRefinement)

      structPropRawTerm <- lift $ transTerm (Crude.unRefinement structureRefinement)

      let structPropTerm =
            substTerm
              _
              structPropRawTerm

      structPropPred <- lift $ reflTerm _

      ctxStructureProperties
        %= ( H.Bind
               { bSym = F.symbol . render $ "witness to assumption of structure property" <+> F.pprint structPropSymbol,
                 bSort = F.FFunc structSort F.boolSort,
                 bPred =
                   H.Reft
                     ( F.PAll
                         [(F.symbol @String "instance", structSort)]
                         ( F.eApps (F.eVar structPropSymbol) [F.eVar @String "instance"]
                             `F.PImp` structPropTerm
                         )
                     ),
                 bMeta = RefiningError $ "intro of witness to assumption of structure property" <+> F.pprint structPropSymbol
               }
               :
           )
  -}

  -- intro variants
  varnts <- asks (^. ctxVariants)
  forM_ (Map.elems varnts) \varnt -> do
    dd <- lift $ reflVariant varnt
    ctxQuery . _qData %= (dd :)

introTransforms :: StateT CheckingCtx RefiningM ()
introTransforms = do
  -- intro transforms as uninterpreted functions (in Query.qCon)
  funs <- asks (^. ctxFunctions)
  forM_ (Map.elems funs) \Function {..} ->
    when functionIsTransform do
      funOutputSort <- lift $ reflType functionOutput
      funSort <- lift $ foldr F.FFunc funOutputSort <$> ((reflType . snd) `traverse` functionParameters)
      ctxQuery . _qCon . at (makeTermIdSymbol functionId) ?= funSort

      -- global assumption that output satisfies the refinement implied by its type

      -- freshen parameter ids, to ensure no naming collisions
      params' <- functionParameters <&*> \(paramId, paramType) -> freshenTermId paramId <&> (,paramType)
      tmexs <-
        lift . execWriterT $
          inferTypeRefinements
            (TermApplication functionId (params' <&> uncurry TermNamed) functionOutput)
            functionOutput
      tmexs' <- error "!TODO universally quantify over parameters in each tmex"

      ctxAssumptionsReversed %= (tmexs' <>)

inferTypeRefinements :: Term -> Type -> WriterT [TermExpr] RefiningM ()
inferTypeRefinements tm (TypeNumber nt n) = do
  case nt of
    Crude.TypeUInt -> do
      -- 0 <= tm
      lowerBoundExpr <- lift . reflTermExpr $ TermPrimitive (PrimitiveLe (intTerm 0 n) tm) TypeBit
      -- tm <= 2^n - 1
      upperBoundExpr <- lift . reflTermExpr $ TermPrimitive (PrimitiveLe tm (intTerm (2 ^ n - 1) n)) TypeBit
      tell [lowerBoundExpr, upperBoundExpr]
    Crude.TypeInt -> do
      -- -(2^(n-1)) + 1 <= tm
      lowerBoundExpr <- lift . reflTermExpr $ TermPrimitive (PrimitiveLe (intTerm (-(2 ^ (n - 1)) + 1) n) tm) TypeBit
      -- tm <= 2^(n-1) - 1
      upperBoundExpr <- lift . reflTermExpr $ TermPrimitive (PrimitiveLe tm (intTerm (2 ^ (n - 1) - 1) n)) TypeBit
      tell [lowerBoundExpr, upperBoundExpr]
    Crude.TypeFloat -> do
      -- -(2^(n-1)) + 1 <= tm
      lowerBoundExpr <- lift . reflTermExpr $ TermPrimitive (PrimitiveLe (floatTerm (-(2 ^ (n - 1)) + 1) n) tm) TypeBit
      -- tm <= 2^(n-1) - 1
      upperBoundExpr <- lift . reflTermExpr $ TermPrimitive (PrimitiveLe tm (floatTerm (2 ^ (n - 1) - 1) n)) TypeBit
      tell [lowerBoundExpr, upperBoundExpr]
inferTypeRefinements _tm TypeBit = return ()
inferTypeRefinements _tm TypeChar = return ()
inferTypeRefinements tm (TypeArray ty) = _w1V
inferTypeRefinements tm (TypeTuple ty1 ty2) = do
  inferTypeRefinements (TermPrimitive (PrimitiveFirst tm) ty1) ty1
  inferTypeRefinements (TermPrimitive (PrimitiveSecond tm) ty2) ty2
inferTypeRefinements tm (TypeOptional ty) = _w1X
inferTypeRefinements tm (TypeNamed ti) = _w1Y

introConstants :: StateT CheckingCtx RefiningM ()
introConstants = do
  -- intro constant as  equation
  asks (^. ctxConstants . to Map.toList) >>= traverse_ \(tmId, tm) -> do
    let sym = makeTermIdSymbol tmId
    tm' <- lift $ transTerm tm
    ex <- lift $ reflTerm tm'
    srt <- lift $ reflType (termType tm')
    (ctxQuery . _qEqns)
      %= ( F.Equ
             { eqName = sym,
               eqArgs = [],
               eqBody = ex,
               eqSort = srt,
               eqRec = False
             }
             :
         )

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
checkBranch matchTerm PatternNone branchTerm = flip localExecM (checkTerm branchTerm) do
  let propTerm = eqTerm matchTerm (TermPrimitive PrimitiveNone (termType matchTerm))
  propPred <- lift $ reflTerm propTerm
  ctxAssumptionsReversed
    %= (TermExpr propTerm propPred :)
checkBranch matchTerm (PatternSome tmId) branchTerm = flip localExecM (checkTerm branchTerm) do
  ty <- case termType matchTerm of
    TypeOptional ty -> return ty
    ty -> FlexM.throw $ "PatternSome should not have matched term with type" <+> ticks (pPrint ty)
  let propTerm = eqTerm matchTerm (TermPrimitive (PrimitiveSome (TermNamed tmId ty)) (termType matchTerm))
  propPred <- lift $ reflTerm propTerm
  ctxAssumptionsReversed
    %= (TermExpr propTerm propPred :)

checkPrimitive :: Type -> Primitive -> CheckingM ()
checkPrimitive _ (PrimitiveTry tm) = checkTerm `traverse_` [tm]
checkPrimitive _ PrimitiveNone = return ()
checkPrimitive _ (PrimitiveSome tm) = checkTerm `traverse_` [tm]
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
checkPrimitive _ (PrimitiveLe tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveLt tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveExtends tm _tyId) = checkTerm `traverse_` [tm]
