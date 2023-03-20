module Language.Flex.Refining.Translating where

import Control.Lens (At (at), locally)
import Control.Monad (void, when)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Functor
import Data.Maybe (fromMaybe)
import qualified Language.Fixpoint.Types as F
import qualified Language.Flex.FlexBug as FlexBug
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Embedding (embedTerm)
import Language.Flex.Refining.RefiningM (RefiningM, ctxTermIdSubstitution, freshId', freshId'TermId, freshSymbol, introApplicantType, introId', lookupApplicantType, lookupFunction, lookupId', throwRefiningError)
import Language.Flex.Refining.Syntax
import Language.Flex.Syntax (Literal (..))
import qualified Language.Flex.Syntax as Base
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), render, (<+>))
import Utility (comps, compsM)

-- ** Translate Term

transTerm :: Base.Term Base.Type -> RefiningM (Term Base.Type)
transTerm term = do
  FlexM.debug . FlexM.FlexLog "refining" $ "[transTerm]" <+> pPrint term
  case term of
    Base.TermLiteral lit ty -> return $ TermLiteral lit ty
    Base.TermPrimitive prim ty ->
      TermPrimitive
        <$> case prim of
          Base.PrimitiveTry te -> PrimitiveTry <$> transTerm te
          Base.PrimitiveTuple tes -> PrimitiveTuple <$> transTerm `traverse` tes
          Base.PrimitiveArray tes -> PrimitiveArray <$> transTerm `traverse` tes
          Base.PrimitiveIf te te' te3 -> PrimitiveIf <$> transTerm te <*> transTerm te' <*> transTerm te3
          Base.PrimitiveAnd te te' -> PrimitiveAnd <$> transTerm te <*> transTerm te'
          Base.PrimitiveOr te te' -> PrimitiveOr <$> transTerm te <*> transTerm te'
          Base.PrimitiveNot te -> PrimitiveNot <$> transTerm te
          Base.PrimitiveEq te te' -> PrimitiveEq <$> transTerm te <*> transTerm te'
          Base.PrimitiveAdd te te' -> PrimitiveAdd <$> transTerm te <*> transTerm te'
          -- invalid
          Base.PrimitiveCast _ -> FlexBug.throw $ FlexM.FlexLog "refining" $ "PrimitiveCast should not appear in typed term:" <+> pPrint term
        <*> return ty
    Base.TermLet {termPattern, termTerm, termBody} -> do
      id' <- case termPattern of
        Base.PatternNamed ti _ty -> freshId'TermId ti
        Base.PatternDiscard _ty -> freshId' "discard"
      tm <- transTerm termTerm
      ty <- transType $ getTermTopR tm
      bod <-
        introId' id'
          . introApplicantType id' (Base.ApplicantType ty)
          $ transTerm termBody
      return $ TermLet id' tm bod (getTermTopR bod)
    Base.TermAssert {termTerm, termBody} -> do
      tm <- transTerm termTerm
      bod <- transTerm termBody
      return $ TermAssert tm bod (getTermTopR bod)
    Base.TermStructure _ti _x0 _ty -> error "transTerm"
    Base.TermMember _te _fi _ty -> error "transTerm"
    Base.TermNeutral app mb_args mb_cxargs ty -> do
      id' <- lookupId' (void app)
      -- note that we avoid inserting the globally-known refinement types here,
      -- since we will do that in the refining step anyway, and we must provide a
      -- `Base.Type` right now anyway since we're producing a `Term Base.Type`
      lookupApplicantType id' >>= \case
        -- function application is inlined
        Base.ApplicantTypeFunction Base.FunctionType {..} | not functionTypeIsTransform -> do
          mb_args' <- (transTerm `traverse`) `traverse` mb_args
          mb_cxargs' <- (transTerm `traverse`) `traverse` mb_cxargs
          lookupFunction id' >>= \Base.Function {..} -> do
            -- substitute params for args
            maybe
              id
              ( \args' ->
                  comps
                    ( (functionParameters `zip` args') <&> \((tmId, _ty), arg) ->
                        locally (ctxTermIdSubstitution . at tmId) (const $ Just arg)
                    )
              )
              mb_args'
              $
              -- substitute contextual params for contextual args
              maybe
                id
                ( \(cxargs', cxparams) ->
                    comps
                      ( (cxparams `zip` cxargs') <&> \((_tyId, tmId), cxarg) ->
                          locally (ctxTermIdSubstitution . at tmId) (const $ Just cxarg)
                      )
                )
                ((,) <$> mb_cxargs' <*> functionContextualParameters)
              $ transTerm functionBody
        _ -> do
          mb_args' <- (transTerm `traverse`) `traverse` mb_args
          mb_cxargs' <- (transTerm `traverse`) `traverse` mb_cxargs
          let args'' = fromMaybe [] mb_args' ++ fromMaybe [] mb_cxargs'
          return $ TermNeutral id' args'' ty
    Base.TermMatch _te _x0 _ty -> error "transTerm"
    -- invalid
    Base.TermAscribe _te _ty _ty' -> FlexBug.throw $ FlexM.FlexLog "refining" $ "term ascribe should not appear in typed term:" <+> pPrint term

-- ** Translate Type

transType :: Base.Type -> RefiningM Type
transType type_ = case type_ of
  Base.TypeNumber numty n -> do
    x <- freshSymbol (render $ pPrint type_)
    let p = case numty of
          Base.TypeInt ->
            -- -2^(n-1) < x < 2^(n-1)
            F.conj
              [ F.PAtom F.Le (F.expr (-(2 ^ (n - 1)) :: Int)) (F.expr x),
                F.PAtom F.Lt (F.expr x) (F.expr (2 ^ (n - 1) :: Int))
              ]
          Base.TypeUInt ->
            -- 0 <= x < 2^n
            F.conj
              [ F.PAtom F.Le (F.expr (0 :: Int)) (F.expr x),
                F.PAtom F.Lt (F.expr x) (F.expr (2 ^ n :: Int))
              ]
          Base.TypeFloat -> error "transLiteral TypeFloat"
    let atomic = case numty of
          Base.TypeInt -> TypeInt
          Base.TypeUInt -> TypeInt
          Base.TypeFloat -> TypeFloat
    return $ TypeAtomic atomic (F.reft x p)
  Base.TypeBit -> return $ TypeAtomic TypeBit F.trueReft
  Base.TypeChar -> return $ TypeAtomic TypeChar F.trueReft
  Base.TypeArray Base.TypeChar -> return $ TypeAtomic TypeString F.trueReft
  Base.TypeArray _ty -> error "transType TODO"
  Base.TypeTuple tys -> typeTuple =<< transType `traverse` tys
  Base.TypeOptional _ty -> error "transType TODO"
  Base.TypeNamed _ti -> error "transType TODO"
  Base.TypeStructure _struc -> error "transType TODO"
  Base.TypeEnum _en -> error "transType TODO"
  Base.TypeVariant _vari -> error "transType TODO"
  Base.TypeNewtype _new -> error "transType TODO"
  -- invalid
  Base.TypeUnifyVar _ _ -> FlexBug.throw $ FlexM.FlexLog "refining" $ "type unification variable should not appear in normalized type:" <+> pPrint type_

-- ** Basic Types

-- | Refined tuple type.
--
-- > typeTuple [.., { xI: aI | pI(xI) }, ...] = { tuple: (..., aI, ...) | (tuple == (..., xI, ....)) && ... && pI(xI) && .... }
typeTuple :: [Type] -> RefiningM Type
typeTuple tyComps = do
  -- unrefined type, for embedding
  let tyTuple = TypeTuple tyComps mempty

  -- p1: { tuple == (..., yI, ....) }
  tuple <- freshSymbol "tuple"
  p1 <-
    eqPred
      (termVar (fromSymbolToId' tuple) tyTuple)
      ( unrefinedTermTuple $
          tyComps <&> \tyComp ->
            fromSymbolToTerm (F.reftBind $ getTypeTopR tyComp) tyComp
      )

  -- p2: ... && rI(yI) && ...
  let p2 = F.conj $ tyComps <&> (F.reftPred . getTypeTopR)

  -- r: { tuple | tuple == (..., yI, ...) && ... && rI(yI) && ... }
  let r = F.reft tuple (F.conj [p1, p2])

  -- { tuple: (..., aI, ...) | tuple == (..., yI, ...) && ... && rI(yI) && ... }
  return (TypeTuple tyComps r)

-- ** Utilities

-- | The predicate that asserts that two (embedded) terms are equal.
--
-- > eqPred tm1 tm2 = { tm1 == tm2 }
eqPred :: Term Type -> Term Type -> RefiningM F.Pred
eqPred tm1 tm2 =
  embedTerm $
    TermPrimitive
      (PrimitiveEq tm1 tm2)
      (typeBit mempty)

-- *** Translating to Sorts

-- -- TODO:DEPRECATED: is this still needed?
-- sortOfBaseType :: Base.Type -> F.Sort
-- sortOfBaseType = \case
--   Base.TypeNumber nt _n -> case nt of
--     Base.TypeInt -> F.intSort
--     Base.TypeUInt -> F.intSort
--     Base.TypeFloat -> F.realSort
--   Base.TypeBit -> F.boolSort
--   Base.TypeChar -> F.charSort
--   -- TODO: use FApp (type constructor application) and FObj (uninterpreted
--   -- type), and don't need to worry about needing to directly convert TypeIds to
--   -- Symbols since there's never any possible shadoing of TypeIds
--   Base.TypeArray _ty -> error "sortOfBaseType"
--   Base.TypeTuple _tys -> error "sortOfBaseType"
--   Base.TypeOptional _ty -> error "sortOfBaseType"
--   Base.TypeNamed _ti -> error "sortOfBaseType"
--   Base.TypeUnifyVar _uv _m_uc -> error "sortOfBaseType"
--   Base.TypeStructure _struc -> error "sortOfBaseType"
--   Base.TypeEnum _en -> error "sortOfBaseType"
--   Base.TypeVariant _vari -> error "sortOfBaseType"
--   Base.TypeNewtype _new -> error "sortOfBaseType"
