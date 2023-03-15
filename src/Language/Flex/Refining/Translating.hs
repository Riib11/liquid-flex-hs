module Language.Flex.Refining.Translating where

import Control.Monad (when)
import qualified Language.Fixpoint.Types as F
import qualified Language.Flex.FlexBug as FlexBug
import Language.Flex.FlexM (FlexLog (FlexLog))
import Language.Flex.Refining.RefiningM (RefiningM, freshSymbol, throwRefiningError)
import Language.Flex.Refining.Syntax
import Language.Flex.Syntax (Literal (..))
import qualified Language.Flex.Syntax as Base
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), render, (<+>))

transTerm :: Base.Term Base.Type -> RefiningM (Term Base.Type)
transTerm term = case term of
  Base.TermLiteral lit ty -> return $ TermLiteral lit ty
  Base.TermPrimitive prim ty ->
    TermPrimitive
      <$> case prim of
        Base.PrimitiveTry te -> PrimitiveTry <$> transTerm te
        Base.PrimitiveCast _ -> FlexBug.throw $ FlexLog "refining" $ "PrimitiveCast should not appear in typed term:" <+> pPrint term
        Base.PrimitiveTuple tes -> PrimitiveTuple <$> transTerm `traverse` tes
        Base.PrimitiveArray tes -> PrimitiveArray <$> transTerm `traverse` tes
        Base.PrimitiveIf te te' te3 -> PrimitiveIf <$> transTerm te <*> transTerm te' <*> transTerm te3
        Base.PrimitiveAnd te te' -> PrimitiveAnd <$> transTerm te <*> transTerm te'
        Base.PrimitiveOr te te' -> PrimitiveOr <$> transTerm te <*> transTerm te'
        Base.PrimitiveNot te -> PrimitiveNot <$> transTerm te
        Base.PrimitiveEq te te' -> PrimitiveEq <$> transTerm te <*> transTerm te'
        Base.PrimitiveAdd te te' -> PrimitiveAdd <$> transTerm te <*> transTerm te'
      <*> return ty
  -- inlines local definitions
  Base.TermBlock (stmts, tm) _ty -> go stmts
    where
      go :: [Base.Statement Base.Type] -> RefiningM (Term Base.Type)
      go [] = transTerm tm
      go ((Base.StatementLet (Base.PatternNamed ti _ty') te) : stmts') = substTerm ti <$> transTerm te <*> go stmts'
      go ((Base.StatementLet (Base.PatternDiscard _ty') _te) : stmts') = go stmts'
      go ((Base.StatementAssert te) : stmts') = do
        te' <- transTerm te
        body' <- go stmts'
        return $ TermAssert te' body' (getTermR body')
  Base.TermStructure _ti _x0 _ty -> error "transTerm"
  Base.TermMember _te _fi _ty -> error "transTerm"
  Base.TermNeutral (Base.Applicant (Nothing, tmId)) Nothing Nothing ty -> return $ TermNamed tmId ty
  -- TODO: inline functions
  Base.TermNeutral _ap _m_tes _ma _ty -> error "transTerm"
  Base.TermMatch _te _x0 _ty -> error "transTerm"
  -- invalid
  Base.TermAscribe _te _ty _ty' -> FlexBug.throw $ FlexLog "refining" $ "term ascribe should not appear in typed term:" <+> pPrint term

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
    let at = case numty of
          Base.TypeInt -> TypeInt
          Base.TypeUInt -> TypeInt
          Base.TypeFloat -> TypeFloat
    return $ TypeAtomic at (F.reft x p)
  Base.TypeBit -> return $ TypeAtomic TypeBit F.trueReft
  Base.TypeChar -> return $ TypeAtomic TypeChar F.trueReft
  Base.TypeArray Base.TypeChar -> return $ TypeAtomic TypeString F.trueReft
  Base.TypeArray _ty -> error "transType TODO"
  Base.TypeTuple _tys -> error "transType TODO"
  Base.TypeOptional _ty -> error "transType TODO"
  Base.TypeNamed _ti -> error "transType TODO"
  Base.TypeStructure _struc -> error "transType TODO"
  Base.TypeEnum _en -> error "transType TODO"
  Base.TypeVariant _vari -> error "transType TODO"
  Base.TypeNewtype _new -> error "transType TODO"
  -- invalid
  Base.TypeUnifyVar _ _ -> FlexBug.throw $ FlexLog "refining" $ "type unification variable should not appear in normalized type:" <+> pPrint type_
