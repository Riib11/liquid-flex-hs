module Language.Flex.Refining.Logic where

import qualified Language.Fixpoint.Types as F

-- | `F.conj` but first filters out trivial conjuncts.
conjPred :: [F.Pred] -> F.Pred
conjPred [] = F.conj []
conjPred ps
  | null ps' = F.PTrue
  | otherwise = F.conj ps'
  where
    ps' = filter (not . F.isTautoPred) ps
