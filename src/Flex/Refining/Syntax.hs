module Flex.Refining.Syntax where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq
import Control.Exception
import Control.Monad (foldM, void, when)
import Data.Bifunctor (second)
import qualified Data.List as List
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
import PrettyShow (PrettyShow (prettyShow))
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
  | AtomicFloat
  | AtomicBit
  | AtomicChar
  | AtomicString
  deriving (Eq, Show)

instance Show r => PrettyShow (BaseType_ r) where
  prettyShow = \case
    TypeAtomic r at -> case at of
      AtomicInt -> "int{" <> show r <> "}"
      AtomicFloat -> "float{" <> show r <> "}"
      AtomicBit -> "bit{" <> show r <> "}"
      AtomicChar -> "char{" <> show r <> "}"
      AtomicString -> "string{" <> show r <> "}"

trueBaseType :: BaseType
trueBaseType = bitBaseType F.trueReft

falseBaseType :: BaseType
falseBaseType = bitBaseType F.falseReft

intBaseType :: F.Reft -> BaseType
intBaseType r = TypeAtomic r AtomicInt

bitBaseType :: F.Reft -> BaseType
bitBaseType r = TypeAtomic r AtomicBit

charBaseType :: F.Reft -> BaseType
charBaseType r = TypeAtomic r AtomicBit

stringBaseType :: F.Reft -> BaseType
stringBaseType r = TypeAtomic r AtomicString

-- | FunType
--
-- Liquid Flex's function types are simple in that the they cannot express
-- dependency of one type's refinement on a preceeding parameter's value.
type FunType = FunType_ F.Reft

data FunType_ r
  = FunType ![BaseType_ r] !(BaseType_ r)
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
  syms (FunType _params outTy) = F.syms outTy
  substa f (FunType params outTy) = FunType params (F.substa f outTy)
  substf f (FunType params outTy) = FunType params (F.substf f outTy)
  subst f (FunType params outTy) = FunType params (F.subst f outTy)

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

instance PrettyShow Term where
  prettyShow = prettyShow . termPreterm

data Preterm
  = TermLiteral !Literal
  | TermVar !F.Symbol
  | TermBlock !Block
  | TermApp !App [Term]
  deriving (Eq, Show)

instance PrettyShow Preterm where
  prettyShow = \case
    TermLiteral lit -> prettyShow lit
    TermVar x -> show x
    TermBlock block -> prettyShowBlock block
    TermApp app args -> prettyShow app <> "(" <> List.intercalate ", " (prettyShow <$> args) <> ")"

data App
  = AppPrimFun Syn.PrimFun
  | AppVar F.Symbol
  deriving (Eq, Show)

instance PrettyShow App where
  prettyShow = \case
    AppPrimFun pf -> case pf of
      Syn.PrimFunEq -> "=="
      Syn.PrimFunOr -> "||"
      Syn.PrimFunAnd -> "&&"
      Syn.PrimFunNot -> "!"
    AppVar x -> show x

type Block = ([Statement], Term)

prettyShowBlock :: Block -> String
prettyShowBlock (stmts, tm) = "{" <> go stmts <> "}"
  where
    go [] = prettyShow tm
    go (stmt : stmts') = prettyShow stmt <> "; " <> go stmts'

data Statement
  = StatementLet !F.Symbol !Term
  | StatementAssert !Term
  deriving (Eq, Show)

instance PrettyShow Statement where
  prettyShow = \case
    StatementLet x tm -> show x <> " = " <> show tm
    StatementAssert tm -> "assert(" <> prettyShow tm <> ")"

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

-- | parsing
parseSymbol :: String -> F.Symbol
parseSymbol = FP.doParse' FP.lowerIdP "parseSymbol"

parsePred :: String -> F.Pred
parsePred = FP.doParse' FP.predP "parsePred"