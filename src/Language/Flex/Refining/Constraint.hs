{-# LANGUAGE TemplateHaskell #-}

module Language.Flex.Refining.Constraint where

import Control.Lens
import Control.Monad (void)
import Control.Monad.Reader (MonadReader, asks)
import qualified Data.Map as Map
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Types as F
import Language.Flex.FlexM (FlexM, MonadFlex)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Logic (replaceSym)
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import Language.Flex.Refining.Translating (embedType, makeBind)
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), nest, render, vcat, ($$), (<+>))
import Utility (pprintInline, renderInline, (<$$>))

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
  FlexM.debug False $ "forall" <+> F.pprint x <+> ":" <+> pPrint ty
  s <- embedType ty
  let qr = setQReftBind x $ typeAnn ty
  let quants = qreftQuants qr
  $(FlexM.debugThing False [|F.pprint|] [|x|])
  $(FlexM.debugThing False [|pPrint|] [|quants|])
  return $
    -- prefix with all the constraints in @ty@
    foldr
      quantCstr
      (H.All (makeBind x s (H.Reft (qreftPred qr))) cstr)
      quants

-- TODO: include context in error message

-- | `Head` is a special constructor relevant to Horn Clauses, so read more
-- about Horn clauses to learn where this detail is relevant.
cstrHead :: MonadReader RefiningCtx m => Term TypeReft -> TypeReft -> F.Pred -> m Cstr
cstrHead tmSynth tyExpect pSpec = do
  appTypes <- asks (^. ctxApplicantTypes)
  return $
    H.Head (H.Reft pSpec) . RefiningError . vcat $
      [ "",
        "the term",
        nest 2 . vcat $
          [ "with nested refinements:",
            nest 2 $ pPrint tmSynth,
            "",
            "without nested refinements:",
            nest 2 $ pPrint (void <$> tmSynth)
          ],
        "",
        "was synthesized to sastisfy the refined type\n",
        nest 2 $ pPrint (termAnn tmSynth),
        "",
        "but it was expected to satisfy the refined type\n",
        nest 2 $ pPrint tyExpect,
        "",
        "in the context:",
        "",
        nest 2 . vcat $
          Map.toList appTypes <&> \(symId, tr) ->
            pPrint symId <+> ":" <+> pPrint tr
      ]

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
