module Language.Flex.Refining.Constraint where

import qualified Language.Fixpoint.Horn.Types as H
import Language.Fixpoint.Types (pprint)
import qualified Language.Fixpoint.Types as F
import Language.Flex.Refining.RefiningM (RefiningError (RefiningError))
import Language.Flex.Refining.Syntax
import Language.Flex.Refining.Translating (sortOfType)
import Language.Flex.Refining.Types
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), (<+>))

trivialCstr :: Cstr
trivialCstr = H.CAnd []

andCstr :: Cstr -> Cstr -> Cstr
andCstr cstr1 cstr2 = andCstrs [cstr1, cstr2]

andCstrs :: [Cstr] -> Cstr
andCstrs = H.CAnd

-- | The constraint that `forall { x: a | p(x) }, q(x)`
--
-- > cstrForall x { y: a | p(y) } q(x) = forall { x: a | p(x) }, q(x)
cstrForall :: F.Symbol -> Type -> Cstr -> Cstr
cstrForall x ty = H.All (H.Bind x s p (RefiningError "cstrForall"))
  where
    (s, p) = predReplacingBind x ty

-- | `Head` is a special constructor relevant to Horn Clauses, so read more
-- about Horn clauses to learn where this detail is relevant.
cstrHead :: F.Expr -> Cstr
cstrHead e = H.Head (H.Reft e) (RefiningError $ "Subtype error:" <+> pprint e)

-- | The sorted and predicate `(a, p(x))`
--
-- > predReplacingBind x { y: a | p(y) } = (a, p(x))
predReplacingBind :: F.Symbol -> Type -> (F.Sort, H.Pred)
predReplacingBind x ty =
  ( sortOfType ty,
    let r = getTypeTopR ty
     in H.Reft $ subst (F.reftPred r) (F.reftBind r) x
  )
