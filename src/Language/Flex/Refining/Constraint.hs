module Language.Flex.Refining.Constraint where

import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Types as F
import Language.Flex.FlexM (FlexM)
import Language.Flex.Refining.RefiningM (RefiningError (RefiningError))
import Language.Flex.Refining.Syntax
import Language.Flex.Refining.Translating (embedType)
import Language.Flex.Refining.Types
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), nest, vcat, ($$), (<+>))

trivialCstr :: Cstr
trivialCstr = H.CAnd []

andCstr :: Cstr -> Cstr -> Cstr
andCstr cstr1 cstr2 = andCstrs [cstr1, cstr2]

andCstrs :: [Cstr] -> Cstr
andCstrs = H.CAnd

-- | The constraint that `forall { x: a | p(x) }, q(x)`
--
-- > cstrForall x { y: a | p(y) } q(x) = forall { x: a | p(x) }, q(x)
cstrForall :: F.Symbol -> TypeReft -> Cstr -> FlexM Cstr
cstrForall x ty cstr = do
  (s, p) <- predReplacingBind x ty
  return $ H.All (H.Bind x s p (RefiningError "cstrForall")) cstr

-- | `Head` is a special constructor relevant to Horn Clauses, so read more
-- about Horn clauses to learn where this detail is relevant.
cstrHead :: Term TypeReft -> F.Expr -> TypeReft -> F.Expr -> Cstr
cstrHead tmSynth _eSynth tyExpect eExpect =
  H.Head
    (H.Reft eExpect)
    ( RefiningError $
        "the term\n"
          $$ nest 2 (pPrint tmSynth)
          $$ ""
          $$ "was synthesized to sastisfy the refined type\n"
          $$ nest 2 (pPrint (termAnn tmSynth))
          $$ ""
          $$ "but it was expected to satisfy the refined type\n"
          $$ nest 2 (pPrint tyExpect)
          $$ ""
    )

-- | The sorted and predicate `(a, p(x))`
--
-- > predReplacingBind x { y: a | p(y) } = (a, p(x))
predReplacingBind :: F.Symbol -> TypeReft -> FlexM (F.Sort, H.Pred)
predReplacingBind x ty = do
  ty' <- embedType ty
  return
    ( ty',
      let r = typeAnn ty
       in H.Reft $ subst (F.reftPred r) (F.reftBind r) x
    )
