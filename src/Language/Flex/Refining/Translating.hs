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
transTerm = error "TODO"

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
  Base.TypeArray _ty -> error "transType TypeArray"
  Base.TypeTuple _tys -> error "transType TODO"
  Base.TypeOptional _ty -> error "transType TODO"
  Base.TypeNamed _ti -> error "transType TODO"
  Base.TypeStructure _struc -> error "transType TODO"
  Base.TypeEnum _en -> error "transType TODO"
  Base.TypeVariant _vari -> error "transType TODO"
  Base.TypeNewtype _new -> error "transType TODO"
  -- invalid
  Base.TypeUnifyVar _ _ -> FlexBug.throw $ FlexLog "refining" $ "type unification variable should not appear in normalized type:" <+> pPrint type_
