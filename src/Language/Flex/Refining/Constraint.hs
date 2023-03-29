{-# LANGUAGE TemplateHaskell #-}

module Language.Flex.Refining.Constraint where

import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Types as F
import Language.Flex.FlexM (FlexM, MonadFlex)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Logic (replaceSym)
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import Language.Flex.Refining.Translating (embedType, makeBind)
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), nest, render, vcat, ($$), (<+>))
import Utility (pprintInline, renderInline)

trivialCstr :: Cstr
trivialCstr = H.CAnd []

andCstr :: Cstr -> Cstr -> Cstr
andCstr cstr1 cstr2 = andCstrs [cstr1, cstr2]

andCstrs :: [Cstr] -> Cstr
andCstrs = H.CAnd

-- | The constraint that `forall { x: a | p(x) }, q(x)`
--
-- > cstrForall x { y: a | p(y) } q(x) = forall { x: a | p(x) }, q(x)
cstrForall :: MonadFlex m => F.Symbol -> TypeReft -> Cstr -> m Cstr
-- cstrForall x ty cstr = FlexM.markSection [FlexM.FlexMarkStep "cstrForall" . Just $ "forall" <+> F.pprint x <+> ":" <+> pPrint ty] do
cstrForall x ty cstr = FlexM.markSection [FlexM.FlexMarkStep "cstrForall" Nothing] do
  FlexM.debug True $ "forall" <+> F.pprint x <+> ":" <+> pPrint ty
  s <- embedType ty
  let qr = setQReftBind x $ typeAnn ty
  let quants = qreftQuants qr
  $(FlexM.debugThing True [|F.pprint|] [|x|])
  $(FlexM.debugThing True [|pPrint|] [|quants|])
  return $
    -- prefix with all the constraints in @ty@
    foldr
      quantCstr
      (H.All (makeBind x s (H.Reft (qreftPred qr))) cstr)
      quants

-- | `Head` is a special constructor relevant to Horn Clauses, so read more
-- about Horn clauses to learn where this detail is relevant.
cstrHead :: Term TypeReft -> TypeReft -> F.Pred -> Cstr
cstrHead tmSynth tyExpect pSpec =
  H.Head
    (H.Reft pSpec)
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

-- -- | The sorted and predicate `(a, p(x))`
-- --
-- -- > predReplacingBind x { y: a | p(y) } = (a, p(x))
-- predReplacingBind :: MonadFlex m => F.Symbol -> TypeReft -> m (F.Sort, H.Pred)
-- predReplacingBind x ty = do
--   ty' <- embedType ty
--   return
--     ( ty',
--       let r = typeAnn ty
--        in H.Reft $ subst (F.reftPred r) (F.reftBind r) x
--     )
