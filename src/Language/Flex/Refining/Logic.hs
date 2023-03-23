module Language.Flex.Refining.Logic where

import qualified Language.Fixpoint.Types as F

-- | `F.conj` but first filters out trivial conjuncts.
conjPred :: [F.Pred] -> F.Pred
conjPred ps = F.conj (filter (not . F.isTautoPred) ps)
