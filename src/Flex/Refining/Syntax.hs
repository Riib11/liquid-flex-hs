module Flex.Refining.Syntax where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq
import Control.Exception
import Control.Monad (foldM, void, when)
import Data.Bifunctor (second)
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import Data.Typeable
import Flex.Refining.CG
import Flex.Syntax (Id, Literal, ModuleId)
import qualified Flex.Syntax as Syn
import GHC.Generics
import GHC.IO.Exception (ExitCode)
import qualified Language.Fixpoint.Horn.Solve as HS
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Misc as F
import qualified Language.Fixpoint.Misc as Misc
import qualified Language.Fixpoint.Parse as FP
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.Config as FC
import qualified Language.Fixpoint.Utils.Files as Files
import System.Exit (exitWith)
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
import Text.Printf (printf)
import Utility

{-
Reflection

It's reasonable to define another version of the syntax that has refinements
attached to the types and terms in the appropriate places since the code that
deals with Flex purely syntactically without using Liquid Fixpoint should not
have to touch the Liquid Fixpoint-relevant data. I could in theory make the base
syntax polymorphic over the Liquid Fixpoint-relevant data, but that would
require a bunch of type variables in various places, and I don't even want to
reflect _everything_ from Flex, such as statements a-normal types, etc.
-}

-- | Type
--
-- Size constraints for numeric types are included in refinement info. In basic
-- Flex, function types have contextual parameters as well, but by the time we
-- are doing refinement-type checking, we already know that everything is
-- well-typed, so we can just have normal function types that have already
-- combined the arguments and contextual arguments into the appropriate list of
-- types.
type Type = Type_ F.Reft

data Type_ r
  = TypeBaseType (BaseType_ r)
  | TypeFunType (FunType_ r)
  deriving (Eq, Show)

-- | BaseType
type BaseType = BaseType_ F.Reft

-- TODO: handle more advanced types

-- | TypeTuple r ![Type_ r]
-- | TypeArray r !(Type_ r)
-- | TypeOptional r (Type_ r)
-- | TypeStructure Structure r
-- | TypeEnumerated Enumerated r
-- | TypeVariant Variant r
-- | TypeNewtype NewType_ r
data BaseType_ r
  = TypeAtomic r Atomic
  deriving
    (Eq, Show, Functor)

data Atomic
  = AtomicInt
  | AtomicUInt
  | AtomicFloat
  | AtomicBit
  | AtomicChar
  | AtomicString
  deriving (Eq, Show)

-- | FunType
--
-- Liquid Flex's function types are simple in that the they cannot express
-- dependency of one type's refinement on a preceeding parameter's value.
type FunType = FunType_ F.Reft

data FunType_ r
  = FunType ![(F.Symbol, BaseType_ r)] !(BaseType_ r)
  deriving (Eq, Show)

-- | Subable (Subtypeable)
instance F.Subable r => F.Subable (Type_ r) where
  syms = \case
    TypeBaseType baseTy -> F.syms baseTy
    TypeFunType funTy -> F.syms funTy

  substa f = \case
    TypeBaseType baseTy -> TypeBaseType $ F.substa f baseTy
    TypeFunType funTy -> TypeFunType $ F.substa f funTy
  substf f = \case
    TypeBaseType baseTy -> TypeBaseType $ F.substf f baseTy
    TypeFunType funTy -> TypeFunType $ F.substf f funTy
  subst f = \case
    TypeBaseType baseTy -> TypeBaseType $ F.subst f baseTy
    TypeFunType funTy -> TypeFunType $ F.subst f funTy

instance F.Subable r => F.Subable (BaseType_ r) where
  syms = \case
    TypeAtomic r _ -> F.syms r
  substa f = \case
    TypeAtomic r atomic -> TypeAtomic (F.substa f r) atomic
  substf f = \case
    TypeAtomic r atomic -> TypeAtomic (F.substf f r) atomic
  subst f = \case
    TypeAtomic r atomic -> TypeAtomic (F.subst f r) atomic

instance F.Subable r => F.Subable (FunType_ r) where
  syms (FunType params outTy) =
    concatMap
      (F.syms . snd)
      params
      <> F.syms outTy
  substa f (FunType params outTy) =
    FunType
      (second (F.substa f) <$> params)
      (F.substa f outTy)
  substf f (FunType params outTy) =
    FunType
      (second (F.substf f) <$> params)
      (F.substf f outTy)
  subst f (FunType params outTy) =
    FunType
      (second (F.subst f) <$> params)
      (F.subst f outTy)

-- | Term
--
-- TODO: desc
--
-- TODO: tmp disable advanced terms
-- | TermTuple ![Term]
-- | TermArray ![Term]
-- | TermTuple ![Term]
-- | TermStructure Structure (Map.Map Text Term)
-- | TermMember Type -- Term Text
-- | TermConstructor Type
-- | TermMatch Type
data Term = Term
  { termPreterm :: Preterm,
    termType :: BaseType
  }
  deriving (Eq, Show)

data Preterm
  = TermLit !Literal
  | TermVar !F.Symbol
  | TermBlock !Block
  | TermApp !Appl [Term]
  deriving (Eq, Show)

data Appl
  = ApplPrimFun Syn.PrimFun
  | ApplVar F.Symbol
  deriving (Eq, Show)

type Block = ([Statement], Term)

data Statement
  = StatementLet !F.Symbol !(Maybe BaseType) !Term
  | StatementAssert !Term !BaseType
  deriving (Eq, Show)

instance HasLabel Term where
  getLabel _ = F.dummySpan

-- | Env
type Env = F.SEnv Type

emptyEnv :: Env
emptyEnv = F.emptySEnv

extendEnv :: F.Symbol -> Type -> Env -> Env
extendEnv = F.insertSEnv

lookupEnv :: F.Symbol -> Env -> CG Type
lookupEnv x env = case F.lookupSEnv x env of
  Nothing ->
    throwCG
      [ RefineError $
          "Can't find variable's refinement type in environment: "
            <> show x
      ]
  Just ty -> return ty

-- | Substitution
--
-- substitute `x` for `y` in `thing` via Subable
subst :: F.Subable a => a -> F.Symbol -> F.Symbol -> a
subst thing x y = F.subst (F.mkSubst [(x, F.expr y)]) thing

-- -- TODO: do i need this? could only work on Term(Var|Literal)
-- subst' :: F.Subable a => a -> F.Symbol -> Term -> a
-- subst' thing x y = F.subst sigma thing
--   where
--     sigma = F.mkSubst [(x, embedVar y)]
