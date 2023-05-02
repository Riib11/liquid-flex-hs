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
import qualified Data.Traversable as Traversable
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Types as F
import Language.Flex.FlexM (freshenTermId)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Constraint
import Language.Flex.Refining.Prelude (preludeQuery)
import Language.Flex.Refining.Primitive (option_SomeFieldAccessorLocatedSymbol, optional_SomeConstructorSymbol, primitiveDataDecls, tuple_FirstFieldAccessorSymbol, tuple_SecondFieldAccessorSymbol)
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
  = ScopeForall F.Symbol F.Sort
  | ScopeLet F.Symbol F.Expr F.Sort
  deriving (Show)

instance Pretty ScopeItem where
  pPrint (ScopeForall ti srt) = "forall" <+> F.pprint ti <+> ":" <+> F.pprint srt
  pPrint (ScopeLet ti ex srt) = "let" <+> F.pprint ti <+> "=" <+> F.pprint ex <+> ":" <+> F.pprint srt

makeLenses ''CheckingCtx

runCheckingM :: CheckingM a -> RefiningM a
runCheckingM m = do
  ctx <- initCheckingContext
  runReaderT m ctx

ctxScope :: Functor f => ([_] -> f [_]) -> CheckingCtx -> f CheckingCtx
ctxScope = ctxScopeReversed . reversed

ctxAssumptions :: Functor f => ([_] -> f [_]) -> CheckingCtx -> f CheckingCtx
ctxAssumptions = ctxAssumptionsReversed . reversed

reflTermLocal :: Term -> (F.Expr -> CheckingM a) -> CheckingM a
reflTermLocal tm k = do
  (ex, params) <- lift $ runWriterT $ reflTerm tm
  flip comps (k ex) $
    params <&> \(sym, srt, mb_ex') ->
      local
        ( ctxScopeReversed %~ case mb_ex' of
            Nothing -> (ScopeForall sym srt :)
            Just ex' -> (ScopeLet sym ex' srt :)
        )

reflTermState :: Term -> StateT CheckingCtx RefiningM F.Expr
reflTermState tm = do
  (ex, params) <- lift $ runWriterT $ reflTerm tm
  for_ params \(sym, srt, mb_ex') ->
    ctxScopeReversed %= case mb_ex' of
      Nothing -> (ScopeForall sym srt :)
      Just ex' -> (ScopeLet sym ex' srt :)
  return ex

introBinding :: Crude.TermId -> Term -> CheckingM a -> CheckingM a
introBinding tmId tm = localExecM do
  ex <- reflTermState tm
  srt <- lift $ reflType $ termType tm
  ctxScopeReversed %= (ScopeLet (makeTermIdSymbol tmId) ex srt :)

introBinding' :: F.Symbol -> F.Expr -> F.Sort -> CheckingM a -> CheckingM a
introBinding' sym ex srt = localExecM do
  ctxScopeReversed %= (ScopeLet sym ex srt :)

introForall :: Crude.TermId -> Type -> CheckingM a -> CheckingM a
introForall tmId ty = localExecM do
  srt <- lift $ reflType ty
  ctxScopeReversed %= (ScopeForall (makeTermIdSymbol tmId) srt :)

introForall' :: F.Symbol -> F.Sort -> CheckingM a -> CheckingM a
introForall' sym srt = localExecM do
  ctxScopeReversed %= (ScopeForall sym srt :)

introCase :: Term -> Crude.TypeId -> Crude.TermId -> [Crude.TermId] -> [Type] -> CheckingM a -> CheckingM a
introCase tm tyId ctorId paramIds paramTypes = localExecM do
  paramTypeSorts <- lift $ paramTypes <&*> reflType
  -- !TODO assume refinements implied by types
  ctxScopeReversed %= ((uncurry ScopeForall <$> ((makeTermIdSymbol <$> paramIds) `zip` paramTypeSorts)) <>)

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
  propPred <- reflTermState propTerm
  ctxAssumptionsReversed %= (propPred :)

  return ()

assume :: Term -> CheckingM a -> CheckingM a
assume propTerm = localExecM do
  propPred <- reflTermState propTerm
  ctxAssumptionsReversed %= (propPred :)

assume' :: F.Pred -> CheckingM a -> CheckingM a
assume' p = localExecM do
  ctxAssumptionsReversed %= (p :)

-- When introducing assumption, use implications or univ quant witnesses.
-- It seems that either works?
_assumptions_via_witnesses = False

assert :: Doc -> Term -> CheckingM ()
assert sourceDoc tm = flip localExecM checkQuery do
  ex0 <- reflTermState tm
  ctxAssertion .= ex0

  -- True: use implications
  -- False: use witnesses
  cstr0 <-
    if not _assumptions_via_witnesses
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
              (ScopeForall sym srt) ->
                H.All
                  H.Bind
                    { bSym = sym,
                      bSort = srt,
                      bPred = H.Reft (F.prop True),
                      bMeta = RefiningError $ "intro" <+> F.pprint sym <+> ":" <+> F.pprint srt
                    }
              (ScopeLet sym ex srt) ->
                H.All
                  H.Bind
                    { bSym = sym,
                      bSort = srt,
                      bPred = H.Reft $ F.PAtom F.Eq (F.eVar sym) ex,
                      bMeta = RefiningError $ "intro" <+> F.pprint sym <+> ":" <+> F.pprint srt <+> "=" <+> F.pprint ex
                    }
          )
          cstr0
          scope

  (ctxQuery . _qCstr) .= cstr1

-- ** Initialize Context

initCheckingContext :: RefiningM CheckingCtx
initCheckingContext = do
  query <- lift . lift . lift $ preludeQuery
  let ctx =
        CheckingCtx
          { _ctxQuery = query,
            _ctxScopeReversed = mempty,
            _ctxAssumptionsReversed = mempty,
            _ctxAssertion = F.prop True -- will be overwritten
          }
  flip execStateT ctx $ do
    -- intro primitive properties, like inArray
    introPrimitiveProperties

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

-- intro primitive properties, like inArray
introPrimitiveProperties :: StateT CheckingCtx RefiningM ()
introPrimitiveProperties = return () -- !TODO

introUserDatatypes :: StateT CheckingCtx RefiningM ()
introUserDatatypes = do
  -- intro structures
  structs <- asks (^. ctxStructures)
  forM_ structs introStructureUserDatatype

  -- intro variants
  varnts <- asks (^. ctxVariants)
  forM_ (Map.elems varnts) introVariantUserDatatype

introStructureUserDatatype :: Structure -> StateT CheckingCtx RefiningM ()
introStructureUserDatatype struct = do
  let structId = structureId struct
  symLoc <- FlexM.defaultLocated $ makeTypeIdSymbol structId
  -- intro reflected datatype
  dd <- lift $ reflStructure struct
  ctxQuery . _qData %= (dd :)
  -- @
  -- struct A {
  --   b: B;
  --   assert P(b);
  -- }
  -- predicate isA(a : A) uninterpreted
  -- axiom: forall (a : A) . isA(a) ==> P(a.b) && isB(a.b)
  --
  -- struct B {
  --   y: A;
  --   assert Q(y);
  -- }
  --
  -- predicate isB(b : B) uninterpreted
  -- axiom: forall (b : B) . isB(b) ==> P(b.a) && isA(b.a)
  -- @

  let structSort = F.FTC (F.symbolFTycon symLoc)

  -- Intro uninterpreted predicate.
  -- > propS : S -> Bool
  (ctxQuery . _qCon . at (makeDatatypePropertySymbol structId))
    ?= F.FFunc structSort F.boolSort

  FlexM.debug True $ "structureFields struct =" <+> pPrint (structureFields struct)
  FlexM.debug True $ "structureRefinement struct =" <+> pPrint (structureRefinement struct)

  -- Intro assumption about predicate
  asmpExpr <- do
    structVar <- FlexM.freshSymbol "struct"
    refnTerm <- lift $ transRefinement (structureRefinement struct)
    refnExpr <- do
      (refnExpr0, scope) <- lift $ runWriterT $ reflTerm refnTerm
      -- !TODO this isn't ideal, but it's the only way I could get it to work
      -- inline bound items in scope
      let reflExpr1 =
            F.subst
              ( F.Su $
                  mempty
                    & comps
                      ( scope <&> \(sym, _srt, mb_ex) -> case mb_ex of
                          Nothing -> id
                          Just ex -> at sym ?~ ex
                      )
              )
              refnExpr0
      -- !TODO handle univ quantified items in scope???
      return reflExpr1
    -- replace appearances of each variable with field access from struct var x
    -- > P(x.a, x.b, ...)
    let sigma =
          F.Su $
            mempty
              & comps
                ( structureFields struct <&> \(fieldId, _ty) ->
                    at (makeFieldIdSymbol fieldId)
                      ?~ F.eApps
                        (F.eVar (makeStructureFieldAccessorSymbol (structId, fieldId)))
                        [F.eVar structVar]
                )

    let refnExpr' = F.subst sigma refnExpr

    (params', exs) <-
      lift . runWriterT . execWriterT $
        structureFields struct <&*> \(fieldId, fieldType) -> do
          inferTypeRefinements
            (F.eApps (F.eVar (makeStructureFieldAccessorSymbol (structId, fieldId))) [F.eVar structVar])
            fieldType

    -- > forall (x : S) . isS(x) ==> P(x.a, x.b, ...) && ...
    return $
      F.PAll ([(structVar, structSort)] <> params') $
        F.PImp (F.eApps (F.eVar (makeDatatypePropertySymbol structId)) [F.eVar structVar]) $
          F.pAnd ([refnExpr'] <> exs)
  ctxAssumptionsReversed %= (asmpExpr :)
  return ()

introVariantUserDatatype :: Variant -> StateT CheckingCtx RefiningM ()
introVariantUserDatatype varnt = do
  -- intro reflected datatype
  dd <- lift $ reflVariant varnt
  ctxQuery . _qData %= (dd :)
  -- !TODO intro refinement predicate
  return ()

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
      -- reflect types
      params'' <- lift $ params' <&*> bimapM (pure . makeTermIdSymbol) reflType

      (xs, exs) <-
        lift . runWriterT . execWriterT $
          inferTypeRefinements
            -- (TermApplication functionId (params' <&> uncurry TermNamed) functionOutput)
            (F.eApps (F.eVar (makeTermIdSymbol functionId)) $ params' <&> \(x, _) -> F.eVar (makeTermIdSymbol x))
            functionOutput
      -- in each expr, universally quantify over params'' _and_ xs
      let exs' = exs <&> \ex -> F.PAll (params'' <> xs) ex
      ctxAssumptionsReversed %= (exs' <>)

assumeTypeRefinements :: F.Expr -> Type -> CheckingM a -> CheckingM a
assumeTypeRefinements ex ty m = do
  (params, exs) <-
    lift . runWriterT . execWriterT $
      inferTypeRefinements ex ty
  comps
    [ \m' -> foldr (uncurry introForall') m' params,
      \m' -> foldr assume' m' exs
    ]
    m

inferTypeRefinements :: F.Expr -> Type -> WriterT [(F.Symbol, F.Sort)] (WriterT [F.Expr] RefiningM) ()
inferTypeRefinements ex (TypeNumber nt n) = do
  case nt of
    Crude.TypeUInt -> do
      -- !OLD
      -- -- 0 <= tm
      -- lowerBoundExpr <- lift . lift . reflTerm $ TermPrimitive (PrimitiveLe (intTerm 0 n) tm) TypeBit
      -- -- tm <= 2^n - 1
      -- upperBoundExpr <- lift . lift . reflTerm $ TermPrimitive (PrimitiveLe tm (intTerm (2 ^ n - 1) n)) TypeBit
      -- lift $ tell [lowerBoundExpr, upperBoundExpr]
      let n2 = 2 ^ (n - 1) :: Int
      -- 0 <= ex
      let lowerBoundExpr = F.PAtom F.Le (F.expr (0 :: Int)) ex
      -- ex <= 2^n - 1
      let upperBoundExpr = F.PAtom F.Lt ex (F.expr n2)
      lift $ tell [lowerBoundExpr, upperBoundExpr]
    Crude.TypeInt -> do
      -- !OLD
      -- -- -(2^(n-1)) + 1 <= tm
      -- lowerBoundExpr <- lift . lift . reflTerm $ TermPrimitive (PrimitiveLe (intTerm (-(2 ^ (n - 1)) + 1) n) tm) TypeBit
      -- -- tm <= 2^(n-1) - 1
      -- upperBoundExpr <- lift . lift . reflTerm $ TermPrimitive (PrimitiveLe tm (intTerm (2 ^ (n - 1) - 1) n)) TypeBit
      -- lift $ tell [lowerBoundExpr, upperBoundExpr]
      let n2 = 2 ^ (n - 1) :: Int
      -- -(2^(n-1)) + 1 <= ex
      let lowerBoundExpr = F.PAtom F.Le (F.expr ((-n2) + 1)) ex
      -- ex <= 2^(n-1) - 1
      let upperBoundExpr = F.PAtom F.Lt ex (F.expr (n2 - 1))
      lift $ tell [lowerBoundExpr, upperBoundExpr]
    Crude.TypeFloat -> do
      -- !OLD
      -- -- -(2^(n-1)) + 1 <= tm
      -- lowerBoundExpr <- lift . lift . reflTerm $ TermPrimitive (PrimitiveLe (floatTerm (-(2 ^ (n - 1)) + 1) n) tm) TypeBit
      -- -- tm <= 2^(n-1) - 1
      -- upperBoundExpr <- lift . lift . reflTerm $ TermPrimitive (PrimitiveLe tm (floatTerm (2 ^ (n - 1) - 1) n)) TypeBit
      -- lift $ tell [lowerBoundExpr, upperBoundExpr]
      let n2 = 2 ^ (n - 1) :: Int
      -- -(2^(n-1)) + 1 <= ex
      let lowerBoundExpr = F.PAtom F.Le (F.expr ((-n2) + 1)) ex
      -- ex <= 2^(n-1) - 1
      let upperBoundExpr = F.PAtom F.Lt ex (F.expr (n2 - 1))
      lift $ tell [lowerBoundExpr, upperBoundExpr]
inferTypeRefinements _ex TypeBit = return ()
inferTypeRefinements _ex TypeChar = return ()
-- > ex : Array<A>
-- then induced refinement on `ex` is
-- > forall x : A . inArray(x, ex) ==> inferTypeRefinements x A
inferTypeRefinements _ex (TypeArray _ty) = return () -- !TODO
-- > ex : Tuple<A, B>
-- then induced refinement on `ex` is
-- > inferTypeRefinements (fst ex); inferTypeRefinements (snd ex)
inferTypeRefinements ex (TypeTuple ty1 ty2) = do
  inferTypeRefinements (F.eVar tuple_FirstFieldAccessorSymbol `F.eApps` [ex]) ty1
  inferTypeRefinements (F.eVar tuple_SecondFieldAccessorSymbol `F.eApps` [ex]) ty2
-- > ex : Optional<A>
-- then induced refinement on `ex` is
-- > forall x : A . ex == Some x ==> inferTypeRefinements x A
inferTypeRefinements ex (TypeOptional ty) = do
  someValue <- FlexM.freshSymbol "someValue"
  srt <- lift . lift $ reflType ty
  tell [(someValue, srt)]
  -- any inferred refinements for `somvValue` are under the supposition that
  -- `ex == Some someValue`
  mapWriterT
    ( censor \exs ->
        [ F.PImp
            ( F.PAtom
                F.Eq
                ex
                (F.eVar optional_SomeConstructorSymbol `F.eApps` [F.eVar someValue])
            )
            (F.pAnd exs)
        ]
    )
    do
      inferTypeRefinements (F.eVar option_SomeFieldAccessorLocatedSymbol `F.eApps` [ex]) ty
inferTypeRefinements ex (TypeNamed tyId) = do
  lift . tell $ [F.eVar (makeDatatypePropertySymbol tyId) `F.eApps` [ex]]

introConstants :: StateT CheckingCtx RefiningM ()
introConstants = do
  -- intro constant as  equation
  asks (^. ctxConstants . to Map.toList) >>= traverse_ \(tmId, tm) -> do
    let sym = makeTermIdSymbol tmId
    tm' <- lift $ transTerm tm
    ex <- reflTermState tm'
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
  ( \m -> do
      let f tmId ty =
            comps
              [ assumeTypeRefinements (F.eVar (makeTermIdSymbol tmId)) ty,
                introForall tmId ty
              ]
      foldr (uncurry f) m functionParameters
    )
    $ checkTerm body

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
  propPred <- reflTermState propTerm
  ctxAssumptionsReversed %= (propPred :)
checkBranch matchTerm (PatternSome tmId) branchTerm = flip localExecM (checkTerm branchTerm) do
  ty <- case termType matchTerm of
    TypeOptional ty -> return ty
    ty -> FlexM.throw $ "PatternSome should not have matched term with type" <+> ticks (pPrint ty)
  let propTerm = eqTerm matchTerm (TermPrimitive (PrimitiveSome (TermNamed tmId ty)) (termType matchTerm))
  propPred <- reflTermState propTerm
  ctxAssumptionsReversed %= (propPred :)

checkPrimitive :: Type -> Primitive -> CheckingM ()
checkPrimitive _ (PrimitiveTry tm) = checkTerm `traverse_` [tm]
-- !TODO assert that cast is actually defined given the value being casted
checkPrimitive _ (PrimitiveCast tm _ty1 _ty2) = checkTerm `traverse_` [tm]
checkPrimitive _ PrimitiveNone = return ()
checkPrimitive _ (PrimitiveSome tm) = checkTerm `traverse_` [tm]
checkPrimitive _ (PrimitiveTuple tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveFirst tm) = checkTerm `traverse_` [tm]
checkPrimitive _ (PrimitiveSecond tm) = checkTerm `traverse_` [tm]
checkPrimitive _ (PrimitiveArray tms) = checkTerm `traverse_` tms
checkPrimitive _ (PrimitiveIf tm1 tm2 tm3) = do
  checkTerm tm1
  -- tm1 == true ==> ...
  assume (eqTerm tm1 trueTerm) $ checkTerm tm2
  -- tm1 == false ==> ...
  assume (eqTerm tm1 falseTerm) $ checkTerm tm3
checkPrimitive _ (PrimitiveNot tm) = checkTerm `traverse_` [tm]
checkPrimitive _ (PrimitiveEq _ tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveExtends tm _tyId) = checkTerm `traverse_` [tm]
checkPrimitive _ (PrimitiveBoolBinOp _ tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
-- x % y yields obligation y != 0
checkPrimitive ty prim@(PrimitiveNumBinOp Crude.NumBinOpDiv tm1 tm2) = do
  checkTerm tm1
  assert (pPrint prim) (TermPrimitive (PrimitiveEq False tm2 (TermLiteral (Crude.LiteralInteger 0) ty)) ty)
  checkTerm tm2
-- x / y yields obligation y != 0
checkPrimitive ty prim@(PrimitiveNumBinOp Crude.NumBinOpMod tm1 tm2) = do
  checkTerm tm1
  assert (pPrint prim) (TermPrimitive (PrimitiveEq False tm2 (TermLiteral (Crude.LiteralInteger 0) ty)) ty)
  checkTerm tm2
checkPrimitive _ (PrimitiveNumBinOp _ tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveNumBinRel _ tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
