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
import Utility hiding (foldrM, for)

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
    _ctxAssumptionsReversed :: [F.Expr],
    _ctxAssertion :: F.Expr
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
        nest 2 . vcat $ F.pprint <$> reverse _ctxAssumptionsReversed,
        "assertion:",
        nest 2 $ F.pprint _ctxAssertion
      ]

data ScopeItem
  = ScopeForall Crude.TermId F.Sort
  | ScopeLet Crude.TermId F.Expr F.Sort
  deriving (Show)

instance Pretty ScopeItem where
  pPrint (ScopeForall ti srt) = "forall" <+> pPrint ti <+> ":" <+> F.pprint srt
  pPrint (ScopeLet ti ex srt) = "let" <+> pPrint ti <+> "=" <+> F.pprint ex <+> ":" <+> F.pprint srt

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
  ctxScopeReversed %= (ScopeLet tmId ex srt :)

introForall :: Crude.TermId -> Type -> CheckingM a -> CheckingM a
introForall tmId ty = localExecM do
  srt <- lift $ reflType ty
  ctxScopeReversed %= (ScopeForall tmId srt :)

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
  paramTypeSorts <- lift $ paramTypes <&*> reflType
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
  ctxAssumptionsReversed %= (propPred :)

  return ()

assume :: Term -> CheckingM a -> CheckingM a
assume propTerm = localExecM do
  propPred <- lift $ reflTerm propTerm
  ctxAssumptionsReversed %= (propPred :)

-- when introducing assumption, use implications or univ quant witnesses
_assumptions_via_witnesses = False

assert :: Doc -> Term -> CheckingM ()
assert sourceDoc tm = flip localExecM checkQuery do
  ex0 <- lift $ reflTerm tm
  ctxAssertion .= ex0

  -- True: use implications
  -- False: use witnesses
  cstr0 <-
    if _assumptions_via_witnesses
      then do
        -- wrap in assumptions using F.PImp
        asmpExprs <- gets (^.. ctxAssumptions . traverse)
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
                ( \ex ->
                    H.Any
                      H.Bind
                        { bSym = F.symbol . render $ "wittness to assumption:" <+> ticks (F.pprint ex),
                          bSort = F.boolSort,
                          bPred = H.Reft ex,
                          bMeta = RefiningError $ "assumption:" <+> ticks (F.pprint ex)
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
              (ScopeForall tmId srt) ->
                H.All
                  H.Bind
                    { bSym = makeTermIdSymbol tmId,
                      bSort = srt,
                      bPred = H.Reft (F.prop True),
                      bMeta = RefiningError $ "intro" <+> pPrint tmId <+> ":" <+> F.pprint srt
                    }
              (ScopeLet tmId ex srt) ->
                H.All
                  H.Bind
                    { bSym = makeTermIdSymbol tmId,
                      bSort = srt,
                      bPred = H.Reft $ F.PAtom F.Eq (F.eVar (makeTermIdSymbol tmId)) ex,
                      bMeta = RefiningError $ "intro" <+> pPrint tmId <+> ":" <+> F.pprint srt <+> "=" <+> F.pprint ex
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
            _ctxAssertion = F.prop True -- will be overwritten
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

      -- Make a global assumption that the output satisfies the refinement
      -- implied by its type e.g.
      -- @
      --    struct A {x : int32; assert(0 <= x)}
      --    transform foo(b : bit) -> A { ... }
      -- @
      -- introduces global assumption
      -- @
      --    forall b : bit . isA(foo(b))
      -- @
      -- where `isA` is the global property that corresponds to the refinement
      -- on `struct A`.

      -- freshen parameter ids to ensure no naming collisions
      params' <- lift $ functionParameters <&*> \(paramId, paramType) -> (,) <$> freshenTermId paramId <*> pure paramType
      -- reflect ids and types
      params'' <- lift $ params' <&*> bimapM (pure . makeTermIdSymbol) reflType

      (quals, exs) <-
        lift . runWriterT . execWriterT $
          inferTypeRefinements
            (TermApplication functionId (params' <&> uncurry TermNamed) functionOutput)
            functionOutput
      -- in each expr, universally quantify over params'' _and_ quals
      let exs' = exs <&> \ex -> F.PAll (params'' <> quals) ex
      ctxAssumptionsReversed %= (exs' <>)

-- | Traverse down to atomic types, variants, and structures which have special
-- cases for their inferred refinements.
--
-- @
--   struct S {x: int32; assert(0 <= x)}
-- @
--
-- > inferTypeRefinements x (TupleType S S)
-- > ==> isS(proj1 x) && isS(proj2 x)
--
-- !TODO or maybe don't manually destruct -- can just do the same thing as in `x`
-- case and let the system of equalities figure it out
-- > inferTypeRefinements (TupleTerm x y) (TupleType S S)
-- > ==> isS(x) && isS(y)
inferTypeRefinements :: Term -> Type -> WriterT [(F.Symbol, F.Sort)] (WriterT [F.Expr] RefiningM) ()
inferTypeRefinements tm (TypeNumber nt n) = do
  case nt of
    Crude.TypeUInt -> do
      -- 0 <= tm
      lowerBoundExpr <- lift . lift . reflTerm $ TermPrimitive (PrimitiveLe (intTerm 0 n) tm) TypeBit
      -- tm <= 2^n - 1
      upperBoundExpr <- lift . lift . reflTerm $ TermPrimitive (PrimitiveLe tm (intTerm (2 ^ n - 1) n)) TypeBit
      lift $ tell [lowerBoundExpr, upperBoundExpr]
    Crude.TypeInt -> do
      -- -(2^(n-1)) + 1 <= tm
      lowerBoundExpr <- lift . lift . reflTerm $ TermPrimitive (PrimitiveLe (intTerm (-(2 ^ (n - 1)) + 1) n) tm) TypeBit
      -- tm <= 2^(n-1) - 1
      upperBoundExpr <- lift . lift . reflTerm $ TermPrimitive (PrimitiveLe tm (intTerm (2 ^ (n - 1) - 1) n)) TypeBit
      lift $ tell [lowerBoundExpr, upperBoundExpr]
    Crude.TypeFloat -> do
      -- -(2^(n-1)) + 1 <= tm
      lowerBoundExpr <- lift . lift . reflTerm $ TermPrimitive (PrimitiveLe (floatTerm (-(2 ^ (n - 1)) + 1) n) tm) TypeBit
      -- tm <= 2^(n-1) - 1
      upperBoundExpr <- lift . lift . reflTerm $ TermPrimitive (PrimitiveLe tm (floatTerm (2 ^ (n - 1) - 1) n)) TypeBit
      lift $ tell [lowerBoundExpr, upperBoundExpr]
inferTypeRefinements _tm TypeBit = return ()
inferTypeRefinements _tm TypeChar = return ()
inferTypeRefinements _tm (TypeArray _ty) = error "inferTypeRefinements TypeArray !TODO need to define an 'element of' predicate or something"
inferTypeRefinements tm (TypeTuple ty1 ty2) = do
  inferTypeRefinements (TermPrimitive (PrimitiveFirst tm) ty1) ty1
  inferTypeRefinements (TermPrimitive (PrimitiveSecond tm) ty2) ty2
inferTypeRefinements tm (TypeOptional ty) = do
  -- !TODO
  -- -- if tm == None; no refinements can be yielded from this case
  -- -- if tm == Some x
  -- -- !OLD tmId <- FlexM.freshTermId "someValue"
  -- sym <- FlexM.freshSymbol "someValue"
  -- let tm' = TermNamed tmId ty
  -- srt <- lift . lift $ reflType ty
  -- tell [(sym, srt)] -- forall x : ty . ...
  -- -- if tm == Some x
  -- ex <- lift . lift . reflTerm $ eqTerm tm (TermPrimitive (PrimitiveSome tm') (TypeOptional ty))
  -- lift $ tell [ex]
  -- -- then ...
  -- inferTypeRefinements tm' ty
  pure ()
inferTypeRefinements tm (TypeNamed tyId) =
  lookupType tyId >>= \case
    CtxTypeStructure Structure {..} -> do
      -- !TODO don't do this inline, because can cause infinite recursive
      -- unfolding. instead, refer to top-level predicate that asserts structure
      -- properties of fields (can have cycles of implication among assumptions,
      -- but that's fine)

      -- reftTerm <- lift . lift $ transRefinement structureRefinement
      -- -- e.g. let x = tm.x in R(x) where tm : Structure { x : int32; assert R(x) }
      -- let reftTerm' =
      --       foldr
      --         ( \(fieldId, fieldType) reftTerm'' ->
      --             letTerm
      --               (Crude.fromFieldIdToTermId fieldId)
      --               (TermMember tyId tm fieldId fieldType)
      --               reftTerm''
      --         )
      --         reftTerm
      --         structureFields
      -- reftTermExpr <- lift . lift $ reflTerm reftTerm'
      -- lift . tell $ [reftTermExpr]

      -- !TODO
      let reftTerm = undefined -- TermPredicate (makeStructurePropertySymbol structureId) [tm] TypeBit
      reftTermExpr <- lift . lift $ reflTerm reftTerm
      lift . tell $ [reftTermExpr]

    -- !TODO variants can't be recursive (?), so don't have to worry potentially
    -- infinite unfolding here... but maybe its better overall to do it that
    -- way, so lets delay that choice till i come back to this
    CtxTypeVariant Variant {..} -> do
      -- !TODO
      -- void $
      --   variantConstructors <&*> \(ctorId, ctorTypes) -> do
      --     tmId <- (ctorTypes `zip` [0 ..]) <&*> \(ctorType, i) -> FlexM.freshTermId ("param" <> show i)
      --     tysrts <- lift . lift $ reflType <$*> ctorTypes
      --     -- forall x1, x2, ..., xn
      --     (tmId `zip` tysrts) <&*> \(paramIdSym, paramTypeSort) -> tell [(paramIdSym, paramTypeSort)]
      --     -- if tm == ctor x1 x2 ... xn
      --     -- then ...
      --     (tmId `zip` ctorTypes) <&*> \(paramId, paramType) ->
      --       -- infer type refinements on each of the introduced components
      --       inferTypeRefinements (TermNamed paramId paramType) paramType
      pure ()

-- inferTypeRefinements tm _

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
  FlexM.debug True $ "checkTransform: params =" <+> pPrint functionParameters <+> "; body =" <+> pPrint body
  foldr
    (uncurry introForall)
    -- check body
    (checkTerm body)
    functionParameters

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
  ctxAssumptionsReversed %= (propPred :)
checkBranch matchTerm (PatternSome tmId) branchTerm = flip localExecM (checkTerm branchTerm) do
  ty <- case termType matchTerm of
    TypeOptional ty -> return ty
    ty -> FlexM.throw $ "PatternSome should not have matched term with type" <+> ticks (pPrint ty)
  let propTerm = eqTerm matchTerm (TermPrimitive (PrimitiveSome (TermNamed tmId ty)) (termType matchTerm))
  propPred <- lift $ reflTerm propTerm
  ctxAssumptionsReversed %= (propPred :)

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
