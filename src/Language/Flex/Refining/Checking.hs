module Language.Flex.Refining.Checking where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Foldable (traverse_)
import Data.Maybe
import qualified Language.Fixpoint.Horn.Types as H
import Language.Fixpoint.Types as F
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Constraint
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Reflecting
import Language.Flex.Refining.Syntax
import Language.Flex.Refining.Translating (transRefinement, transTerm)
import qualified Language.Flex.Syntax as Crude
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility

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

-- | Emits a list of constraints that should be conjoined together for checking.
type CheckingM a = WriterT [Cstr] RefiningM a

-- ** Checking

-- | Check all refinement constraints that arise within a term.
checkTerm :: Term -> CheckingM ()
checkTerm (TermLiteral _lit _) =
  return ()
checkTerm (TermPrimitive prim ty) =
  checkPrimitive ty prim
checkTerm (TermLet mb_tmId tm1 tm2 _) = do
  checkTerm tm1
  case mb_tmId of
    Nothing ->
      checkTerm tm2
    Just tmId -> do
      let ty1 = termType tm1
      srt1 <- lift $ reflType ty1
      predEq <- lift $ reflTerm $ eqTerm (TermNamed tmId ty1) tm1
      censor
        ( \cs ->
            [ -- forall { tmId : ty1 | tmId == tm1 } . ...
              H.All
                H.Bind
                  { bSym = F.symbol tmId,
                    bSort = srt1,
                    bPred = H.Reft predEq,
                    bMeta = RefiningError ("introduction via let;" <+> ticks (pPrint tmId <+> "=" <+> pPrint tm1))
                  }
                (H.CAnd cs)
            ]
        )
        $ checkTerm tm2
checkTerm (TermAssert tm1 tm2 _) = do
  ex1 <- lift $ reflTerm tm1
  tell
    [ H.Head
        (H.Reft ex1)
        (RefiningError $ "unable to prove assertion" <+> ticks (pPrint tm1) <+> ".")
    ]
  checkTerm tm2
checkTerm (TermNamed _tmId _) =
  return ()
checkTerm (TermApplication _tmId tms _) =
  checkTerm `traverse_` tms
checkTerm (TermConstructor _varntId _ctorId tms _) =
  checkTerm `traverse_` tms
checkTerm term0@(TermStructure structId fields _) = do
  -- check fields
  checkTerm `traverse_` (snd <$> fields)

  -- check that fields satisfy the structure's refinement
  Structure {..} <- lookupStructure structId
  fields' <- lift $ forM (fields `zip` structureFields) \((fieldId, fieldTerm), (_fieldId, fieldType)) -> do
    fieldExpr <- reflTerm fieldTerm
    fieldSort <- reflType fieldType
    fieldEqPred <- reflTerm $ eqTerm (TermNamed (Crude.fromFieldIdToTermId fieldId) fieldType) fieldTerm
    return (fieldId, fieldTerm, fieldType, fieldExpr, fieldSort, fieldEqPred)
  structPredTerm <- lift $ transRefinement structureRefinement
  structPred <- lift $ reflTerm structPredTerm
  tell
    [ foldr
        ( \(fieldId, fieldTerm, fieldType, _fieldExpr, fieldSort, fieldEqPred) ->
            -- exists { fieldId : fieldType | fieldId == fieldTerm } . ...
            H.Any
              H.Bind
                { bSym = F.symbol fieldId,
                  bSort = fieldSort,
                  bPred = H.Reft fieldEqPred,
                  bMeta = RefiningError $ "introduction (via structure construction refinement check);" <+> pPrint fieldId <+> ":" <+> pPrint fieldType <+> ":=" <+> pPrint fieldTerm
                }
        )
        ( H.Head
            (H.Reft structPred)
            ( RefiningError $
                vcat
                  [ "Unable to prove that structure construction's fields satisfy the structure's refinement.",
                    "Structure construction:",
                    nest 4 (pPrint term0),
                    "Structure refinement:",
                    nest 4 (pPrint structureRefinement)
                  ]
            )
        )
        fields'
    ]
checkTerm (TermMatch tm branches _) = do
  forM_ branches (uncurry (checkBranch tm))

-- !TODO it might be possible here to use `Rewrite`s that get put into the
-- `Query`'s `qMats`, but there's no example of doing this in SPRITE, so I'm not
-- sure if it actually works. If I _was_ to try this, I'd have to:
-- - augment the Writer type to also accumulate `Rewrite`s
-- - instead of using a witness, just tell a `Rewrite`
-- - potential problem: the `Rewrite` will refer to some variables that are
--   bound deeply inside the `Cstr`... so will the `Query` have a problem with
--   that? The `Query` can also accept `Qualifier`s, but the way I've set up my
--   building-up of the `Cstr`, its useful to introduce qualfiers exactly where
--   they are needed rather than pulling them all out to the top.

-- > matchTerm  = `a`
-- > branchPat  = `C x y z`
-- > branchTerm = `b`
-- > constraint = `forall x y z . (C x y z == a)  ==>  checkTerm b`
checkBranch :: Term -> Pattern -> Term -> CheckingM ()
checkBranch matchTerm (PatternConstructor varntId ctorId ctorParamIds) branchTerm = do
  ctorParamTypes <- lift $ lookupConstructorParameterTypes varntId ctorId
  ctorParamSorts <- lift $ reflType `traverse` ctorParamTypes

  -- ctorTerm = ctorId ctorParamId_1 ... ctorParamId_n
  let ctorTerm =
        TermConstructor
          varntId
          ctorId
          (ctorParamIds `zip` ctorParamTypes <&> uncurry TermNamed)
          (TypeNamed varntId)

  -- ctorPropTerm = { matchTerm == ctorTerm }
  let ctorPropTerm = eqTerm matchTerm ctorTerm
  ctorPropPred <- lift $ reflTerm ctorPropTerm

  -- !TODO is it necessary to phrase it in this weird way, or does it work to
  -- phrase it more directly as { wit | True == (match == ctor) }?

  -- witPred = { wit | wit == True && wit == (match == ctor) }
  witSymbol <- FlexM.freshSymbol (render $ "witness of " <+> ticks (pPrint ctorPropTerm))
  let witPred = F.PAtom F.Eq (F.eVar witSymbol) ctorPropPred

  censor
    ( \cs ->
        [ foldr
            ( \((ctorParamId, ctorParamSort), ctorParamType) ->
                -- forall { ctorParam : ctorParamType } . ...
                H.All
                  H.Bind
                    { bSym = F.symbol ctorParamId,
                      bSort = ctorParamSort,
                      bPred = H.Reft (F.prop True),
                      bMeta = RefiningError $ "introduction via match branch;" <+> ticks (pPrint ctorParamId <+> ":" <+> pPrint ctorParamType)
                    }
            )
            ( -- exists { wit : bool | witPred } . ...
              H.Any
                H.Bind
                  { bSym = dummySymbol,
                    bSort = F.boolSort,
                    bPred = H.Reft witPred,
                    bMeta =
                      RefiningError $
                        "assumption via match branch;"
                          <+> ticks (pPrint matchTerm <+> "==" <+> pPrint ctorTerm)
                  }
                $ H.CAnd cs
            )
            (ctorParamIds `zip` ctorParamSorts `zip` ctorParamTypes)
        ]
    )
    $ checkTerm branchTerm

checkPrimitive :: Type -> Primitive -> CheckingM ()
checkPrimitive _ (PrimitiveTry tm) = checkTerm `traverse_` [tm]
checkPrimitive _ (PrimitiveTuple tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveArray tms) = checkTerm `traverse_` tms
checkPrimitive _ (PrimitiveIf tm1 tm2 tm3) = checkTerm `traverse_` [tm1, tm2, tm3]
checkPrimitive _ (PrimitiveAnd tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveOr tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveNot tm) = checkTerm `traverse_` [tm]
checkPrimitive _ (PrimitiveEq tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveAdd tm1 tm2) = checkTerm `traverse_` [tm1, tm2]
checkPrimitive _ (PrimitiveExtends tm _tyId) = checkTerm `traverse_` [tm]
