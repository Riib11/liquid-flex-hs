module Language.Flex.Refining.Syntax where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq
import Control.Exception
import Control.Monad (foldM, void, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Bifunctor (second)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import Data.Typeable
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
import Language.Flex.Syntax (FieldId, Literal (..), TermId, TypeId)
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
import Text.PrettyPrint.HughesPJClass
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

-- ** Type

-- Size constraints for numeric types are included in refinement info. In basic
-- Flex, function types have contextual parameters as well, but by the time we
-- are doing refinement-type checking, we already know that everything is
-- well-typed, so we can just have normal function types that have already
-- combined the arguments and contextual arguments into the appropriate list of
-- types.

-- ** Type

type Type = Type_ F.Reft

-- TODO: handle more advanced types

-- | TypeTuple r ![Type_ r]
-- | TypeArray r !(Type_ r)
-- | TypeOptional r (Type_ r)
-- | TypeStructure Structure r
-- | TypeEnumerated Enumerated r
-- | TypeVariant Variant r
-- | TypeNewtype NewType_ r
data Type_ r
  = TypeAtomic AtomicType r
  deriving
    (Eq, Show, Functor)

instance Pretty (Type_ F.Reft) where
  pPrint = \case
    TypeAtomic at r -> case at of
      TypeInt -> go "int"
      TypeFloat -> go "float"
      TypeBit -> go "bit"
      TypeChar -> go "char"
      TypeString -> go "string"
      where
        go str = braces $ braces $ F.pprint (F.reftBind r) <+> colon <+> str <+> "|" <+> F.pprint (F.reftPred r)

baseTypeReft :: Type -> F.Reft
baseTypeReft = \case
  TypeAtomic _ r -> r

data AtomicType
  = TypeInt
  | TypeFloat
  | TypeBit
  | TypeChar
  | TypeString
  deriving (Eq, Show)

trueType :: Type
trueType = bitType F.trueReft

falseType :: Type
falseType = bitType F.falseReft

intType :: F.Reft -> Type
intType = TypeAtomic TypeInt

bitType :: F.Reft -> Type
bitType = TypeAtomic TypeBit

charType :: F.Reft -> Type
charType = TypeAtomic TypeChar

stringType :: F.Reft -> Type
stringType = TypeAtomic TypeString

mapTypeTopR :: (r -> r) -> Type_ r -> Type_ r
mapTypeTopR f = \case
  TypeAtomic at r -> TypeAtomic at (f r)

mapMTypeTopR :: Monad m => (r -> m r) -> Type_ r -> m (Type_ r)
mapMTypeTopR k = \case
  TypeAtomic at r -> TypeAtomic at <$> k r

getRefinement :: Type_ r -> r
getRefinement = \case
  TypeAtomic _ r -> r

-- ** FunctionType

-- Liquid Flex's function types are simple in that the they cannot express
-- dependency of one type's refinement on a preceeding parameter's value.
type FunctionType = FunctionType_ F.Reft

data FunctionType_ r = FunctionType
  { functionTypeParameters :: ![Type_ r],
    functionTypeOutput :: !(Type_ r)
  }
  deriving (Eq, Show)

-- | Subable (Subtypeable)
instance F.Subable r => F.Subable (Type_ r) where
  syms = \case
    TypeAtomic _ r -> F.syms r
  substa f = \case
    TypeAtomic at r -> TypeAtomic at (F.substa f r)
  substf f = \case
    TypeAtomic at r -> TypeAtomic at (F.substf f r)
  subst f = \case
    TypeAtomic at r -> TypeAtomic at (F.subst f r)

instance F.Subable r => F.Subable (FunctionType_ r) where
  syms (FunctionType _params outTy) = F.syms outTy
  substa f (FunctionType params outTy) = FunctionType params (F.substa f outTy)
  substf f (FunctionType params outTy) = FunctionType params (F.substf f outTy)
  subst f (FunctionType params outTy) = FunctionType params (F.subst f outTy)

-- ** Structure

data Structure = Structure
  { structureId :: TypeId,
    structureIsMessage :: Bool,
    structureMaybeExtensionId :: Maybe TypeId,
    structureFields :: [(FieldId, Type)],
    structureRefinement :: F.Expr
  }
  deriving (Eq, Show)

-- ** Variant

data Variant = Variant
  { variantId :: TypeId,
    variantConstructors :: [(TermId, Maybe [Type])]
  }
  deriving (Eq, Show)

-- ** Enum

data Enum = Enum
  { enumId :: TypeId,
    enumType :: Type,
    enumConstructors :: [(TermId, Literal)]
  }
  deriving (Eq, Show)

-- ** Function

data Function r = Function
  { functionId :: TermId,
    functionIsTransform :: Bool,
    functionParameters :: [(TermId, Type)],
    functionOutput :: Type,
    functionBody :: Term r
  }
  deriving (Eq, Show)

-- ** Constant

data Constant r = Constant
  { constantId :: TermId,
    constantTerm :: Term r,
    constantType :: Type
  }
  deriving (Eq, Show)

data Primitive r
  = PrimitiveTry (Term r)
  | PrimitiveTuple [Term r]
  | PrimitiveArray [Term r]
  | PrimitiveIf (Term r) (Term r) (Term r)
  | PrimitiveAnd (Term r) (Term r)
  | PrimitiveOr (Term r) (Term r)
  | PrimitiveNot (Term r)
  | PrimitiveEq (Term r) (Term r)
  | PrimitiveAdd (Term r) (Term r)
  deriving (Eq, Show, Functor)

-- ** Term

-- TODO: structure, member, construct enum, construct variant, match
data Term r
  = TermNamed TermId r
  | TermSymbol F.Symbol r
  | TermLiteral !Literal r
  | TermPrimitive !(Primitive r) r
  | TermAssert (Term r) (Term r) r
  deriving (Eq, Show, Functor)

getTermR :: Term r -> r
getTermR = \case
  TermNamed _ti r -> r
  TermSymbol _ r -> r
  TermLiteral _lit r -> r
  TermPrimitive _prim r -> r
  TermAssert _te _te' r -> r

mapTermTopR :: (r -> r) -> Term r -> Term r
mapTermTopR f = \case
  TermNamed ti r -> TermNamed ti (f r)
  TermSymbol sym r -> TermSymbol sym (f r)
  TermLiteral lit r -> TermLiteral lit (f r)
  TermPrimitive prim r -> TermPrimitive prim (f r)
  TermAssert tm1 tm2 r -> TermAssert tm1 tm2 (f r)

mapMTermTopR :: Monad m => (r -> m r) -> Term r -> m (Term r)
mapMTermTopR k = \case
  TermNamed ti r -> TermNamed ti <$> k r
  TermSymbol sym r -> TermSymbol sym <$> k r
  TermLiteral lit r -> TermLiteral lit <$> k r
  TermPrimitive prim r -> TermPrimitive prim <$> k r
  TermAssert tm1 tm2 r -> TermAssert tm1 tm2 <$> k r

-- ** Substitution

--
-- substitute `x` for `y` in `thing` via `Subable`
subst :: F.Subable a => a -> F.Symbol -> F.Symbol -> a
subst thing x y = F.subst (F.mkSubst [(x, F.expr y)]) thing

-- -- TODO: do i need this? could only work on Term(Var|Literal)
-- subst' :: F.Subable a => a -> F.Symbol -> Term -> a
-- subst' thing x y = F.subst sigma thing
--   where
--     sigma = F.mkSubst [(x, embedVar y)]

substTerm :: TermId -> Term r -> Term r -> Term r
substTerm tmId tm' term = case term of
  TermLiteral {} -> term
  TermSymbol {} -> term
  TermNamed tmId' _ | tmId' == tmId -> tm'
  TermNamed {} -> term
  TermPrimitive prim r -> flip TermPrimitive r case prim of
    PrimitiveTry te -> PrimitiveTry (go te)
    PrimitiveTuple tes -> PrimitiveTuple (go <$> tes)
    PrimitiveArray tes -> PrimitiveArray (go <$> tes)
    PrimitiveIf te te' te3 -> PrimitiveIf (go te) (go te') (go te3)
    PrimitiveAnd te te' -> PrimitiveAnd (go te) (go te')
    PrimitiveOr te te' -> PrimitiveOr (go te) (go te')
    PrimitiveNot te -> PrimitiveNot (go te)
    PrimitiveEq te te' -> PrimitiveEq (go te) (go te')
    PrimitiveAdd te te' -> PrimitiveAdd (go te) (go te')
  TermAssert te te' r -> TermAssert (go te) (go te') r
  where
    go = substTerm tmId tm'
