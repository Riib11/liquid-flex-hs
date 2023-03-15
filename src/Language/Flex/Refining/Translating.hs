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

transTerm :: Base.Term Base.Type -> RefiningM Term
transTerm term = case term of
  Base.TermLiteral lit ty -> transLiteral lit ty
  Base.TermPrimitive prim ty -> error "TODO"
  Base.TermBlock x0 ty -> error "TODO"
  Base.TermStructure ti x0 ty -> error "TODO"
  Base.TermMember te fi ty -> error "TODO"
  Base.TermNeutral ap m_tes ma ty -> error "TODO"
  Base.TermMatch te x0 ty -> error "transTerm TermMatch"
  -- invalid
  Base.TermAscribe {} -> FlexBug.throw $ FlexLog "refining" ("TermAscribe should not appear in type-checked  term:" <+> pPrint term)

transLiteral :: Literal -> Base.Type -> RefiningM Term
transLiteral lit ty = case lit of
  LiteralInteger i -> case ty of
    Base.TypeNumber numty size -> do
      when (size == 0) $ throwRefiningError $ "literal number has size zero:" <+> pPrint lit
      ty' <- transType ty
      return $
        TermLiteral
          lit
          ( mapTopBaseType
              ( \(F.Reft (x, p)) ->
                  F.Reft
                    ( x,
                      F.conj
                        [ p,
                          F.PAtom F.Eq (F.eVar x) (F.expr i)
                        ]
                    )
              )
              ty'
          )
    _ -> FlexBug.throw $ FlexLog "refining" "LiteralInteger must have type TypeNumber"
  LiteralFloat x -> error "transLiteral LiteralFloat"
  LiteralBit b -> do
    ty' <- transType ty
    -- let F.Reft (x, p)
    error "TODO"
  LiteralChar c -> error "TODO"
  LiteralString s -> error "TODO"
  where
    -- {x | nMin <= }
    boundedIntExpr :: F.Symbol -> Int -> Int -> F.Expr
    boundedIntExpr x nMin nMax =
      F.PAnd
        [ F.PAtom F.Le (F.expr nMin) (F.expr x),
          F.PAtom F.Lt (F.expr x) (F.expr nMax)
        ]

{-

-}

transType :: Base.Type -> RefiningM BaseType
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
  Base.TypeArray ty -> error "transType TypeArray"
  Base.TypeTuple tys -> error "transType xxx"
  Base.TypeOptional ty -> error "transType xxx"
  Base.TypeNamed ti -> error "transType xxx"
  Base.TypeStructure struc -> error "transType xxx"
  Base.TypeEnum en -> error "transType xxx"
  Base.TypeVariant vari -> error "transType xxx"
  Base.TypeNewtype new -> error "transType xxx"
  -- invalid
  Base.TypeUnifyVar _ _ -> FlexBug.throw $ FlexLog "refining" $ "type unification variable should not appear in normalized type:" <+> pPrint type_
