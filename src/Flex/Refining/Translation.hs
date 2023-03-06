{-# LANGUAGE TemplateHaskell #-}

module Flex.Refining.Translation () where

import Control.Lens
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Reader (ReaderT, asks)
import qualified Data.Map as Map
import Data.Text (Text)
import Flex.Flex (FlexError (RefineError), FlexT)
import qualified Flex.Refining.Syntax as Reft
import qualified Flex.Syntax as Base
import qualified Language.Fixpoint.Types as F
import PrettyShow (PrettyShow (prettyShow))
import Utility (fromJust')

data TranslationCtx = TranslationCtx
  { _idSymbols :: Map.Map Base.Id F.Symbol
  }

makeLenses ''TranslationCtx

type T m a = FlexT (ReaderT TranslationCtx m) a

-- | Translation
--
-- Translate terms and types from Flex to LiquidFlex
transTerm :: Monad m => Base.Term -> T m Reft.Term
transTerm tm = case tm ^. Base.termPreterm of
  Base.TermLiteral lit -> transLiteral (fromJust' "TODO" (tm ^. Base.termType)) lit
  Base.TermCast _ -> throwError $ RefineError $ "there should be no `cast`s left after type-checking, yet there is: " <> prettyShow tm
  Base.TermNamed x ->
    asks (^. idSymbols)
      >>= ( \case
              Nothing -> throwError $ RefineError $ "transTerm: can't find translation of id '" <> prettyShow x <> "' in environment"
              Just x -> do
                ty <- transType (fromJust' "TODO" (tm ^. Base.termType))
                pure $ Reft.Term (Reft.TermVar x) ty
          )
        . Map.lookup x
  Base.TermTuple tes -> error "TODO: transTerm TermTuple"
  Base.TermArray tes -> error "TODO: transTerm TermArray"
  Base.TermBlock block -> transBlock (fromJust' "TODO" (tm ^. Base.termType)) block
  Base.TermStructure _id _map -> error "TODO: transTerm TermStructure"
  Base.TermMember _te _txt -> error "TODO: transTerm TermMember"
  Base.TermConstructor _id _m_te -> error "TODO: transTerm TermConstructor"
  Base.TermApplication _id _tes _m_e -> error "TODO: transTerm"
  Base.TermIf te te' te2 -> error "TODO: transTerm"
  Base.TermAscribe te ty -> error "TODO: transTerm"
  Base.TermMatch _te _x0 -> error "TODO: transTerm TermMatch"

transLiteral :: Monad m => Base.Type -> Base.Literal -> T m Reft.Term
transLiteral = undefined

transBlock :: Monad m => Base.Type -> Base.Block -> T m Reft.Term
transBlock = error "TODO"

transType :: Monad m => Base.Type -> T m Reft.BaseType
transType = error "TODO"
