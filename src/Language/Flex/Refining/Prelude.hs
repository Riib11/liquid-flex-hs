module Language.Flex.Refining.Prelude where

import Control.Lens
import Control.Monad.State (execState, gets)
import Data.Foldable (traverse_)
import qualified Data.Foldable as Set
import qualified Data.Set as Set
import qualified Data.Traversable as Traversable
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Types as F
import Language.Flex.FlexM (FlexM)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import Language.Flex.Syntax (NumberType (TypeFloat, TypeInt, TypeUInt))
import Text.PrettyPrint.HughesPJ (render, text, (<+>))
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
  -- qQuals <-
  --   Traversable.for castProperties \(label, qParams, qBody) -> do
  --     qName <- FlexM.freshSymbol label
  --     qPos <- FlexM.defaultSourcePos
  --     return F.Q {qName, qParams, qBody, qPos}
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

casts :: [(F.Symbol, F.Sort)]
casts =
  [ -- using int32 as canonical, but works for any pair of any-sized int/uint
    (makeCastFunctionSymbol (TypeNumber TypeInt 32) (TypeNumber TypeInt 32), F.FFunc F.intSort (F.FFunc F.intSort (F.FFunc F.intSort F.intSort))),
    -- using float32 as canonical, but works for any pair of any-sized float
    (makeCastFunctionSymbol (TypeNumber TypeFloat 32) (TypeNumber TypeFloat 32), F.FFunc F.realSort (F.FFunc F.realSort (F.FFunc F.realSort F.realSort)))
  ]

castProperties :: [F.Pred]
castProperties =
  [ -- forall inf sup x . inf <= x <= sup ==> cast{inf ~> sup}(x) = x
    do
      let ty1 = TypeNumber TypeInt 32
      let ty2 = TypeNumber TypeInt 32
      let inf = F.symbol @String "inf"
      let sup = F.symbol @String "sup"
      let x = F.symbol @String "x"
      let cast = makeCastFunctionSymbol ty1 ty2
      F.PAll [(inf, F.intSort), (sup, F.intSort), (x, F.intSort)]
        $ F.PImp
          ( F.pAnd
              [ -- inf <= x
                F.PAtom F.Le (F.eVar inf) (F.eVar x),
                -- x <= sup
                F.PAtom F.Le (F.eVar x) (F.eVar sup)
              ]
          )
        $
        -- cast{inf ~> sup}(x) = x
        F.PAtom F.Eq (F.eApps (F.eVar cast) [F.eVar inf, F.eVar sup, F.eVar x]) (F.eVar x),
    -- forall inf sup x . inf <= x <= sup ==> cast{inf ~> sup}(x) = x
    do
      let ty1 = TypeNumber TypeFloat 32
      let ty2 = TypeNumber TypeFloat 32
      let inf = F.symbol @String "inf"
      let sup = F.symbol @String "sup"
      let x = F.symbol @String "x"
      let cast = makeCastFunctionSymbol ty1 ty2
      F.PAll [(inf, F.realSort), (sup, F.realSort), (x, F.realSort)]
        $ F.PImp
          ( F.pAnd
              [ -- inf <= x
                F.PAtom F.Le (F.eVar inf) (F.eVar x),
                -- x <= sup
                F.PAtom F.Le (F.eVar x) (F.eVar sup)
              ]
          )
        $
        -- cast{inf ~> sup}(x) = x
        F.PAtom F.Eq (F.eApps (F.eVar cast) [F.eVar inf, F.eVar sup, F.eVar x]) (F.eVar x)
  ] -- !TODO same sort of rule, but for float casts

-- castTypePairs =
--   [ (TypeNumber TypeInt 32, TypeNumber TypeInt 16)
--   ]

-- casts :: [(F.Symbol, F.Sort)]
-- casts =
--   castTypePairs <&> \(ty1, ty2) ->
--     case (ty1, ty2) of
--       (TypeNumber nt1 _n1, TypeNumber nt2 _) -> do
--         let sortOfNumberType nt = case nt of
--               TypeInt -> F.intSort
--               TypeUInt -> F.intSort
--               TypeFloat -> F.realSort
--         ( F.symbol (makeCastFunctionString ty1 ty2),
--           F.FFunc (sortOfNumberType nt1) (sortOfNumberType nt2)
--           )
--       _ -> error "!TODO non-numeric cast properties?"

-- castProperties :: RefiningM [(String, [F.QualParam], F.Expr)]
-- castProperties = do
--   usedCastings <- gets (^. envUsedCastings)
--   FlexM.debug True $ "castProperties.usedCastings:" <+> pPrint (Set.toList usedCastings)
--   return . concat $
--     Set.toList usedCastings <&> \(ty1, ty2) -> do
--       case (ty1, ty2) of
--         (TypeNumber _nt1 _n1, TypeNumber nt2 n2) -> do
--           -- forall x. infimum <= x <= supremum ==> __partialCast_ty1_ty2(x) == x
--           let x = F.symbol @String "x"
--           [ ( makeCastFunctionString ty1 ty2 <> "_property_witness",
--               [F.QP {qpSym = x, qpPat = F.PatNone, qpSort = F.intSort}],
--               F.PImp
--                 ( F.pAnd
--                     [ F.PAtom F.Le (infimum nt2 n2) (F.eVar x),
--                       F.PAtom F.Le (F.eVar x) (supremum nt2 n2)
--                     ]
--                 )
--                 ( F.PAtom
--                     F.Eq
--                     (F.eVar (makeCastFunctionSymbol ty1 ty2) `F.eApps` [F.eVar x])
--                     (F.eVar x)
--                 )
--             )
--             ]
--         _ -> []
--   where

infimum :: NumberType -> Integer -> F.Expr
infimum TypeUInt _n = F.expr (0 :: Int)
infimum TypeInt n = F.expr (-(2 ^ n - 1) :: Int)
infimum TypeFloat n = F.ECon . F.R $ -(2 ^ n - 1)

supremum :: NumberType -> Integer -> F.Expr
supremum TypeUInt n = F.expr (2 ^ n - 1 :: Int)
supremum TypeInt n = F.expr (2 ^ n - 1 :: Int)
supremum TypeFloat n = F.ECon . F.R $ 2 ^ n - 1

-- there is only one cast function for casting between all valid pairs of
-- numeric types

-- F.symbol @String "cast__int__int"
makeCastFunctionSymbol :: Type -> Type -> F.Symbol
makeCastFunctionSymbol (TypeNumber nt1 _n) (TypeNumber nt2 _n2) | Set.all (`Set.member` Set.fromList [TypeInt, TypeUInt]) [nt1, nt2] = F.symbol @String "__cast_int_int"
makeCastFunctionSymbol (TypeNumber nt1 _n) (TypeNumber nt2 _n2) | Set.all (`Set.member` Set.fromList [TypeFloat]) [nt1, nt2] = F.symbol @String "__cast_float_float"
makeCastFunctionSymbol ty1 ty2 = error . render $ "impossible cast: " <> pPrint ty1 <> ", " <> pPrint ty2

makeCast :: Type -> Type -> F.Expr -> F.Expr
makeCast ty1 ty2@(TypeNumber nt2 i2) e =
  F.eApps
    (F.eVar (makeCastFunctionSymbol ty1 ty2))
    [ infimum nt2 i2,
      supremum nt2 i2,
      e
    ]
makeCast ty1 ty2 _ = error . render $ "impossible cast: " <> pPrint ty1 <> ", " <> pPrint ty2

-- F.symbol $ makeCastFunctionString ty1 ty2

-- makeCastFunctionString :: Type -> Type -> String
-- makeCastFunctionString ty1 ty2 = "__partialCast_" <> typeStr ty1 <> "_" <> typeStr ty2
--   where
--     typeStr :: Type -> String
--     typeStr (TypeNumber nt n) = render $ pPrint nt <> text (show n)
--     typeStr TypeBit = render $ pPrint TypeBit
--     typeStr TypeChar = render $ pPrint TypeChar
--     typeStr (TypeNamed tyId) = render $ pPrint tyId
--     typeStr ty = error . render $ "[makeCasttFunctionSymbol] not sure how to handle creating cast function symbol for type: " <> pPrint ty
