module Language.Flex.Refining.Constraint where

import qualified Language.Fixpoint.Horn.Types as H
import Language.Fixpoint.Types (pprint)
import qualified Language.Fixpoint.Types as F
import Language.Flex.Refining.RefiningM (RefiningError (RefiningError))
import Language.Flex.Refining.Syntax
import Language.Flex.Refining.Types
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), (<+>))

trivialCstr :: Cstr
trivialCstr = H.CAnd []

andCstr :: Cstr -> Cstr -> Cstr
andCstr cstr1 cstr2 = andCstrs [cstr1, cstr2]

andCstrs :: [Cstr] -> Cstr
andCstrs = H.CAnd

forallCstr :: F.Symbol -> Type -> Cstr -> Cstr
forallCstr x ty cstr = case sortPred x ty of
  Just (srt, prd) -> H.All (H.Bind x srt prd (RefiningError "forallCstr")) cstr
  _ -> cstr

-- subtyping constraint (??)
headCstr :: F.Expr -> Cstr
headCstr e = H.Head (H.Reft e) (RefiningError $ "Subtype error:" <+> pprint e)

reftSymbol :: F.Reft -> F.Symbol
reftSymbol (F.Reft (x, _)) = x

reftExpr :: F.Reft -> F.Expr
reftExpr (F.Reft (_, e)) = e

sortPred :: F.Symbol -> Type -> Maybe (F.Sort, H.Pred)
sortPred x = \case
  TypeAtomic atom r ->
    Just
      ( case atom of
          TypeInt -> F.intSort
          TypeFloat -> F.realSort
          TypeBit -> F.boolSort
          TypeChar -> F.charSort
          TypeString -> F.strSort,
        H.Reft (subst (reftExpr r) (reftSymbol r) x)
      )