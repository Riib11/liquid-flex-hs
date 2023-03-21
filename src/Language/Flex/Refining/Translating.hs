module Language.Flex.Refining.Translating where

import Control.Lens (At (at), locally, (^.))
import Control.Monad (foldM, forM, void, when)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Foldable (foldlM)
import Data.Functor
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Language.Fixpoint.Types as F
import qualified Language.Flex.FlexBug as FlexBug
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Embedding (embedTerm, sortOfType)
import Language.Flex.Refining.RefiningM (RefiningM, freshId', freshId'TermId, freshSymbol, freshenBind, freshenTermId, introApplicantType, introBinding, introId', lookupApplicantType, lookupFunction, lookupId', throwRefiningError)
import Language.Flex.Refining.Syntax
import Language.Flex.Syntax (Literal (..))
import qualified Language.Flex.Syntax as Base
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), render, (<+>))
import Utility (comps, compsM, foldrM)

-- ** Translate Term

transTerm :: Base.Term Base.Type -> RefiningM (Term Base.Type)
transTerm term = do
  FlexM.debug . FlexM.FlexLog "refining" $ "[transTerm]" <+> pPrint term
  case term of
    Base.TermLiteral lit ty -> return $ TermLiteral lit ty
    Base.TermPrimitive prim ty ->
      case prim of
        Base.PrimitiveTry te -> TermPrimitive <$> (PrimitiveTry <$> transTerm te) <*> return ty
        Base.PrimitiveTuple tes | length tes < 2 -> FlexBug.throw $ FlexM.FlexLog "refining" "attempted to transTerm on a Base.PrimitiveTuple that has length terms < 2"
        Base.PrimitiveTuple (te : tes) -> do
          te' <- transTerm te
          let f :: Term Base.Type -> Base.Term Base.Type -> RefiningM (Term Base.Type)
              f tm1' tm2 = do
                tm2' <- transTerm tm2
                return $
                  TermPrimitive
                    (PrimitiveTuple (tm1', tm2'))
                    (Base.TypeTuple [getTermTopR tm1', getTermTopR tm2'])
          foldlM f te' tes -- TUPLE: fold left
        Base.PrimitiveTuple _ -> error "IMPOSSIBLE"
        Base.PrimitiveArray tes -> TermPrimitive <$> (PrimitiveArray <$> transTerm `traverse` tes) <*> return ty
        Base.PrimitiveIf te te' te3 -> TermPrimitive <$> (PrimitiveIf <$> transTerm te <*> transTerm te' <*> transTerm te3) <*> return ty
        Base.PrimitiveAnd te te' -> TermPrimitive <$> (PrimitiveAnd <$> transTerm te <*> transTerm te') <*> return ty
        Base.PrimitiveOr te te' -> TermPrimitive <$> (PrimitiveOr <$> transTerm te <*> transTerm te') <*> return ty
        Base.PrimitiveNot te -> TermPrimitive <$> (PrimitiveNot <$> transTerm te) <*> return ty
        Base.PrimitiveEq te te' -> TermPrimitive <$> (PrimitiveEq <$> transTerm te <*> transTerm te') <*> return ty
        Base.PrimitiveAdd te te' -> TermPrimitive <$> (PrimitiveAdd <$> transTerm te <*> transTerm te') <*> return ty
        -- invalid
        Base.PrimitiveCast _ -> FlexBug.throw $ FlexM.FlexLog "refining" $ "PrimitiveCast should not appear in typed term:" <+> pPrint term
    -- local binding is added to refinement context during refinement checking,
    -- not translation (since the implementation of the let needs to be checked
    -- first)
    Base.TermLet {termPattern, termTerm, termBody} -> do
      id' <- case termPattern of
        Base.PatternNamed ti _ty -> freshId'TermId ti
        Base.PatternDiscard _ty -> freshId' "discard"
      tm <- transTerm termTerm
      ty <- transType $ getTermTopR tm
      bod <-
        comps
          [ introId' id',
            introApplicantType id' (Base.ApplicantType ty)
          ]
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
        -- Function application is inlined. For example, given
        --
        -- @
        --    function f(x: bit) -> bit {
        --      let y = !x;
        --      y
        --    }
        -- @
        --
        -- then the application @f(true)@ is inlined to be
        --
        -- @
        --    let x' = true;
        --    let y' = !x';
        --    y'
        -- @
        --
        -- where @x'@ and @y'@ are fresh variables substituted in for @x@
        -- and @y@
        Base.ApplicantTypeFunction Base.FunctionType {..} | not functionTypeIsTransform -> do
          mb_args' <- (transTerm `traverse`) `traverse` mb_args
          mb_cxargs' <- (transTerm `traverse`) `traverse` mb_cxargs
          lookupFunction id' >>= \Base.Function {..} -> do
            -- make fresh versions of arg ids
            -- RefiningM (Map.Map Base.TermId Base.TermId)
            freshArgIds <-
              Map.fromList
                <$> case mb_args' of
                  Nothing -> return []
                  Just args' ->
                    forM (functionParameters `zip` args') \((tmId, _ty), arg') -> do
                      tmId' <- freshenTermId tmId
                      return (tmId, tmId')

            -- make fresh version of cxarg ids
            freshCxargIds <-
              Map.fromList <$> case (functionContextualParameters, mb_cxargs') of
                (Nothing, Nothing) -> return []
                (Just cxparams, Just cxargs') -> forM (cxparams `zip` cxargs') \((_tyId, tmId), _cxarg) -> do
                  tmId' <- freshenTermId tmId
                  return (tmId, tmId')
                _ -> FlexBug.throw $ FlexM.FlexLog "refining" $ "function type's contextual parameters doesn't correspond to application's contextual arguments: " <+> pPrint functionContextualParameters <+> "," <+> pPrint mb_cxargs'

            let freshTermIds = Map.union freshArgIds freshCxargIds

            -- substitute via `freshTermIds` in `functionBody`
            let funBody' = error "TODO:checkpoint"

            transTerm functionBody
        _ -> do
          mb_args' <- (transTerm `traverse`) `traverse` mb_args
          mb_cxargs' <- (transTerm `traverse`) `traverse` mb_cxargs
          let args'' = fromMaybe [] mb_args' ++ fromMaybe [] mb_cxargs'
          return $ TermNeutral id' args'' ty
    Base.TermMatch _te _x0 _ty -> error "transTerm"
    -- invalid
    Base.TermAscribe _te _ty _ty' -> FlexBug.throw $ FlexM.FlexLog "refining" $ "term ascribe should not appear in typed term:" <+> pPrint term

-- ** Translate Type

-- in the refinement, accounts for local bindings as existentially quantified
-- variables with an equality constraint
transType :: Base.Type -> RefiningM TypeReft
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
          Base.TypeFloat -> error "TODO: transType TypeFloat"
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
-- > typeTuple [.., { xI: aI | pI(xI) }, ...] = { tuple: (..., aI, ...) | (tuple
-- > == (..., xI, ....)) && ... && pI(xI) && .... }
typeTuple :: [TypeReft] -> RefiningM TypeReft
typeTuple tys_ = do
  let go :: TypeReft -> TypeReft -> RefiningM TypeReft
      go ty1 ty2 = do
        -- ty1: { x1: a1 | r1(x1) }
        -- ty2: { x2: a2 | r2(x2) }

        -- r1(x1)
        -- r2(x2)
        let r1 = getTypeTopR ty1
            r2 = getTypeTopR ty2

        -- tyTuple: (ty1, ty2)
        -- unrefined, since only used for embedding
        let tyTuple = TypeTuple (void ty1, void ty2) ()

        -- p1(x1, x2): tuple == (x1, x2)
        tuple <- freshSymbol "tuple"
        p1 <-
          eqPred
            (termVar (fromSymbolToId' tuple) tyTuple)
            ( TermPrimitive
                ( PrimitiveTuple
                    ( fromSymbolToTerm (F.reftBind r1) (void ty1),
                      fromSymbolToTerm (F.reftBind r2) (void ty2)
                    )
                )
                tyTuple
            )

        -- p2(x1, x2): r1(x1) && r2(x2)
        let p2 = F.conj $ [ty1, ty2] <&> (F.reftPred . getTypeTopR)

        -- r: { tuple: tyTuple | exists x1 x2 . p1(y1, x2) && p2(y1, x2) }
        let r =
              F.reft tuple $
                F.pExist [(F.reftBind r1, sortOfType ty1), (F.reftBind r2, sortOfType ty2)] $
                  F.conj [p1, p2]

        -- { tuple: (a1, a2) | p1 && p2 }
        return $ TypeTuple (ty1, ty2) r

  case tys_ of
    [] -> error "typeTuple []"
    [_] -> error "typeTuple [ _ ]"
    (ty : tys) -> foldlM go ty tys -- TUPLE: fold left

-- ** Utilities

-- | The predicate that asserts that two (embedded) terms are equal.
--
-- > eqPred tm1 tm2 = { tm1 == tm2 }
eqPred :: Term (Type_ ()) -> Term (Type_ ()) -> RefiningM F.Pred
eqPred tm1 tm2 =
  embedTerm $
    TermPrimitive
      (PrimitiveEq tm1 tm2)
      (typeBit ())

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
