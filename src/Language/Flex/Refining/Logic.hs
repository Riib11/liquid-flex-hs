module Language.Flex.Refining.Logic where

import Control.Monad.Writer
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Types as F

-- | `F.conj` but first filters out trivial conjuncts.
conjPred :: [F.Pred] -> F.Pred
conjPred [] = F.conj []
conjPred ps
  | null ps' = F.PTrue
  | otherwise = F.conj ps'
  where
    ps' = filter (not . F.isTautoPred) ps

-- x := y
replaceSym :: F.Symbol -> F.Symbol -> (F.Symbol -> F.Symbol)
replaceSym x y z = if z == x then y else z

-- !TODO actually, probably need to do this more fundamentally in TypeReft

-- fromPredToCstr :: F.Pred -> Cstr
-- fromPredToCstr prd0 = foldr ($) cstr fs
--   where
--     (cstr, fs) = runWriter $ go prd0
--     go :: F.Pred -> Writer [Cstr -> Cstr] Cstr
--     go prd = case prd of
--       F.ESym _sc -> return $ H.Head (H.Reft prd) _
--       F.ECon _con -> return prd
--       F.EVar _sym -> return prd
--       F.EApp ex ex' -> F.EApp <$> go ex <*> go ex'
--       F.ENeg ex -> F.ENeg <$> go ex
--       F.EBin bop ex ex' -> F.EBin bop <$> go ex <*> go ex'
--       F.EIte ex ex' ex2 -> F.EIte <$> go ex <*> go ex' <*> go ex2
--       F.ECst ex so -> F.ECst <$> go ex <*> return so
--       F.ELam x1 ex -> F.ELam x1 <$> go ex
--       F.ETApp ex so -> F.ETApp <$> go ex <*> return so
--       F.ETAbs ex sym -> F.ETAbs <$> go ex <*> return sym
--       F.PAnd exs -> F.PAnd <$> go `traverse` exs
--       F.POr exs -> F.POr <$> go `traverse` exs
--       F.PNot ex -> F.PNot <$> go ex
--       F.PImp ex ex' -> F.PImp <$> go ex <*> go ex'
--       F.PIff ex ex' -> F.PIff <$> go ex <*> go ex'
--       F.PAtom br ex ex' -> F.PAtom br <$> go ex <*> go ex'
--       F.PKVar _kv _su -> return prd
--       F.PAll x1 ex -> tell [F.PAll x1] >> go ex
--       F.PExist x1 ex -> tell [F.pExist x1] >> go ex
--       F.PGrad kv su gi ex -> F.PGrad kv su gi <$> go ex
--       F.ECoerc so so' ex -> F.ECoerc so so' <$> go ex

-- -- This isn't a logically valid transformation in general, but may be allowed
-- -- under assumptions of where I let quantifiers appear.
-- frontloadQuantifiers :: F.Pred -> F.Pred
-- frontloadQuantifiers prd0 = uncurry (foldr ($)) (runWriter $ go prd0)
--   where
--     go :: F.Pred -> Writer [F.Pred -> F.Pred] F.Pred
--     go prd = case prd of
--       F.ESym _sc -> return prd
--       F.ECon _con -> return prd
--       F.EVar _sym -> return prd
--       F.EApp ex ex' -> F.EApp <$> go ex <*> go ex'
--       F.ENeg ex -> F.ENeg <$> go ex
--       F.EBin bop ex ex' -> F.EBin bop <$> go ex <*> go ex'
--       F.EIte ex ex' ex2 -> F.EIte <$> go ex <*> go ex' <*> go ex2
--       F.ECst ex so -> F.ECst <$> go ex <*> return so
--       F.ELam x1 ex -> F.ELam x1 <$> go ex
--       F.ETApp ex so -> F.ETApp <$> go ex <*> return so
--       F.ETAbs ex sym -> F.ETAbs <$> go ex <*> return sym
--       F.PAnd exs -> F.PAnd <$> go `traverse` exs
--       F.POr exs -> F.POr <$> go `traverse` exs
--       F.PNot ex -> F.PNot <$> go ex
--       F.PImp ex ex' -> F.PImp <$> go ex <*> go ex'
--       F.PIff ex ex' -> F.PIff <$> go ex <*> go ex'
--       F.PAtom br ex ex' -> F.PAtom br <$> go ex <*> go ex'
--       F.PKVar _kv _su -> return prd
--       F.PAll x1 ex -> tell [F.PAll x1] >> go ex
--       F.PExist x1 ex -> tell [F.pExist x1] >> go ex
--       F.PGrad kv su gi ex -> F.PGrad kv su gi <$> go ex
--       F.ECoerc so so' ex -> F.ECoerc so so' <$> go ex
