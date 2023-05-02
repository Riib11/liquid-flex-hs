module Language.Flex.Refining.Prelude where

import Control.Lens
import Control.Monad.State (execState)
import Data.Foldable (traverse_)
import qualified Data.Traversable as Traversable
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Types as F
import Language.Flex.FlexM (FlexM)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import Language.Flex.Syntax (NumberType (TypeFloat, TypeInt, TypeUInt))
import Text.PrettyPrint.HughesPJ (render, text)
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))

preludeRefiningCtx :: RefiningCtx
preludeRefiningCtx =
  RefiningCtx
    { _ctxTypes = mempty,
      _ctxStructures = mempty,
      _ctxVariants = mempty,
      _ctxFunctions = mempty,
      _ctxConstants = mempty
    }

-- do as expressions with foralls rather than as witness style
preludeAssumptions :: FlexM [F.Expr]
preludeAssumptions = do
  return [] -- !TODO

preludeQuery :: FlexM (H.Query w)
preludeQuery = do
  qQuals <-
    Traversable.for castProperties \(label, qParams, qBody) -> do
      qName <- FlexM.freshSymbol label
      qPos <- FlexM.defaultSourcePos
      return F.Q {qName, qParams, qBody, qPos}
  return
    H.Query
      { qQuals = mempty,
        qVars = mempty,
        qCstr = H.CAnd [], -- will be overwritten during checking
        qCon = flip execState mempty do
          -- partial cast functions
          traverse_ (\(sym, srt) -> at sym ?= srt) casts,
        qDis = mempty,
        qEqns = mempty,
        qMats = mempty,
        qData = mempty
      }
  where
    castTypePairs =
      [ (TypeNumber TypeInt 32, TypeNumber TypeInt 16)
      ]

    casts :: [(F.Symbol, F.Sort)]
    casts =
      castTypePairs <&> \(ty1, ty2) ->
        case (ty1, ty2) of
          (TypeNumber nt1 _n1, TypeNumber nt2 _) -> do
            let sortOfNumberType nt = case nt of
                  TypeInt -> F.intSort
                  TypeUInt -> F.intSort
                  TypeFloat -> F.realSort
            ( F.symbol (makeCastFunctionString ty1 ty2),
              F.FFunc (sortOfNumberType nt1) (sortOfNumberType nt2)
              )
          _ -> error "!TODO non-numeric cast properties?"

    infimum :: NumberType -> Integer -> F.Expr
    infimum TypeUInt _n = F.expr (0 :: Int)
    infimum TypeInt n = F.expr (-(2 ^ n - 1) :: Int)
    infimum TypeFloat n = F.ECon . F.R $ -(2 ^ n - 1)

    supremum :: NumberType -> Integer -> F.Expr
    supremum TypeUInt n = F.expr (2 ^ n - 1 :: Int)
    supremum TypeInt n = F.expr (2 ^ n - 1 :: Int)
    supremum TypeFloat n = F.ECon . F.R $ 2 ^ n - 1

    castProperties :: [(String, [F.QualParam], F.Expr)]
    castProperties =
      castTypePairs <&> \(ty1, ty2) -> do
        case (ty1, ty2) of
          (TypeNumber _nt1 _n1, TypeNumber nt2 n2) -> do
            -- forall x. infimum <= x <= supremum ==> __partialCast_ty1_ty2(x) == x
            let x = F.symbol @String "x"
            ( makeCastFunctionString ty1 ty2 <> "_property_witness",
              [F.QP {qpSym = x, qpPat = F.PatNone, qpSort = F.intSort}],
              F.PImp
                ( F.pAnd
                    [ F.PAtom F.Le (infimum nt2 n2) (F.eVar x),
                      F.PAtom F.Le (F.eVar x) (supremum nt2 n2)
                    ]
                )
                ( F.PAtom
                    F.Eq
                    (F.eVar (makeCastFunctionSymbol ty1 ty2) `F.eApps` [F.eVar x])
                    (F.eVar x)
                )
              )
          _ -> error "!TODO non-numeric cast properties?"

makeCastFunctionSymbol :: Type -> Type -> F.Symbol
makeCastFunctionSymbol ty1 ty2 = F.symbol $ makeCastFunctionString ty1 ty2

makeCastFunctionString :: Type -> Type -> String
makeCastFunctionString ty1 ty2 = "__partialCast_" <> typeStr ty1 <> "_" <> typeStr ty2
  where
    typeStr :: Type -> String
    typeStr (TypeNumber nt n) = render $ pPrint nt <> text (show n)
    typeStr TypeBit = render $ pPrint TypeBit
    typeStr TypeChar = render $ pPrint TypeChar
    typeStr (TypeNamed tyId) = render $ pPrint tyId
    typeStr ty = error . render $ "[makeCasttFunctionSymbol] not sure how to handle creating cast function symbol for type: " <> pPrint ty