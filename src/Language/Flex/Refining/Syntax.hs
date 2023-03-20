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
import Text.PrettyPrint.HughesPJClass hiding ((<>))
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

-- | TypeTuple
-- | TypeArray
-- | TypeOptional
-- | TypeStructure
-- | TypeEnumerated
-- | TypeVariant
-- | TypeNewtype
data Type_ r
  = TypeAtomic AtomicType r
  | TypeTuple ![Type_ r] r
  deriving
    (Eq, Show, Functor, Foldable, Traversable)

instance Pretty (Type_ F.Reft) where
  pPrint = \case
    TypeAtomic at r -> case at of
      TypeInt -> go "int" r
      TypeFloat -> go "float" r
      TypeBit -> go "bit" r
      TypeChar -> go "char" r
      TypeString -> go "string" r
    TypeTuple tys r -> go (parens $ hsep $ punctuate comma $ pPrint <$> tys) r
    where
      go doc r = braces $ F.pprint (F.reftBind r) <+> ":" <+> doc <+> "|" <+> F.pprint (F.reftPred r)

baseTypeReft :: Type -> F.Reft
baseTypeReft = \case
  TypeAtomic _ r -> r
  TypeTuple _ r -> r

data AtomicType
  = TypeInt
  | TypeFloat
  | TypeBit
  | TypeChar
  | TypeString
  deriving (Eq, Show)

getTypeTopR :: Type_ r -> r
getTypeTopR = \case
  TypeAtomic _at r -> r
  TypeTuple _tys r -> r

setTypeTopR :: Type_ r -> r -> Type_ r
setTypeTopR ty r = case ty of
  TypeAtomic at _r' -> TypeAtomic at r
  TypeTuple tys _r' -> TypeTuple tys r

mapTypeTopR :: (r -> r) -> Type_ r -> Type_ r
mapTypeTopR f ty = setTypeTopR ty (f (getTypeTopR ty))

mapMTypeTopR :: Functor f => (r -> f r) -> Type_ r -> f (Type_ r)
mapMTypeTopR k ty = setTypeTopR ty <$> k (getTypeTopR ty)

typeTrue :: Type
typeTrue = typeBit F.trueReft

typeFalse :: Type
typeFalse = typeBit F.falseReft

typeInt :: F.Reft -> Type
typeInt = TypeAtomic TypeInt

typeBit :: F.Reft -> Type
typeBit = TypeAtomic TypeBit

typeChar :: F.Reft -> Type
typeChar = TypeAtomic TypeChar

typeString :: F.Reft -> Type
typeString = TypeAtomic TypeString

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
  syms = foldMap F.syms
  substa f = fmap (F.substa f)
  substf f = fmap (F.substf f)
  subst f = fmap (F.subst f)

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
    constantBody :: Term r,
    constantType :: Type
  }
  deriving (Eq, Show)

-- ** Primitive

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

instance Pretty (Primitive r) where
  pPrint = \case
    PrimitiveTry te -> parens $ text "try" <+> pPrint te
    PrimitiveTuple tes -> parens $ hsep $ punctuate comma $ pPrint <$> tes
    PrimitiveArray tes -> brackets $ hsep $ punctuate comma $ pPrint <$> tes
    PrimitiveIf te te' te'' -> parens $ text "if" <+> pPrint te <+> text "then" <+> pPrint te' <+> text "else" <+> pPrint te''
    PrimitiveAnd te te' -> parens $ pPrint te <+> text "&&" <+> pPrint te'
    PrimitiveOr te te' -> parens $ pPrint te <+> text "||" <+> pPrint te'
    PrimitiveNot te -> parens $ text "!" <+> pPrint te
    PrimitiveEq te te' -> parens $ pPrint te <+> "==" <+> pPrint te'
    PrimitiveAdd te te' -> parens $ pPrint te <+> "+" <+> pPrint te'

unrefinedTermTuple :: [Term Type] -> Term Type
unrefinedTermTuple tms =
  TermPrimitive
    (PrimitiveTuple tms)
    (TypeTuple (getTermTopR <$> tms) mempty)

-- ** Term

-- TODO: structure, member, construct enum, construct variant, match
data Term r
  = TermNeutral !Id' [Term r] r
  | TermLiteral !Literal r
  | TermPrimitive !(Primitive r) r
  | TermLet !Id' !(Term r) !(Term r) r
  | TermAssert !(Term r) !(Term r) r
  deriving (Eq, Show, Functor)

instance Pretty (Term r) where
  pPrint = \case
    TermNeutral id' tes _r
      | null tes -> pPrint id'
      | otherwise -> pPrint id' <> parens (hsep $ punctuate comma $ pPrint <$> tes)
    TermLiteral lit _r -> pPrint lit
    TermPrimitive prim _r -> pPrint prim
    TermLet id' te te' _r -> text "let" <+> pPrint id' <+> text "=" <+> pPrint te <+> ";" $$ pPrint te'
    TermAssert te te' _r -> text "assert" <+> pPrint te <+> ";" $$ pPrint te'

data Id' = Id'
  { id'Symbol :: !F.Symbol,
    id'MaybeTermId :: !(Maybe TermId)
  }
  deriving (Eq, Ord, Show)

instance Pretty Id' where
  pPrint (Id' sym _m_ti) = F.pprint sym

termVar :: Id' -> r -> Term r
termVar id' = TermNeutral id' []

fromSymbolToTerm :: F.Symbol -> r -> Term r
fromSymbolToTerm sym = termVar (fromSymbolToId' sym)

fromSymbolToId' :: F.Symbol -> Id'
fromSymbolToId' id'Symbol = Id' {id'Symbol, id'MaybeTermId = Nothing}

getTermTopR :: Term r -> r
getTermTopR = \case
  TermNeutral _ _ r -> r
  TermLiteral _lit r -> r
  TermPrimitive _prim r -> r
  TermAssert _te _te' r -> r
  TermLet _ _ _ r -> r

mapTermTopR :: (r -> r) -> Term r -> Term r
mapTermTopR f = \case
  TermNeutral ti args r -> TermNeutral ti args (f r)
  TermLiteral lit r -> TermLiteral lit (f r)
  TermPrimitive prim r -> TermPrimitive prim (f r)
  TermAssert tm1 tm2 r -> TermAssert tm1 tm2 (f r)
  TermLet x tm1 tm2 r -> TermLet x tm1 tm2 (f r)

mapMTermTopR :: Monad m => (r -> m r) -> Term r -> m (Term r)
mapMTermTopR k = \case
  TermNeutral ti args r -> TermNeutral ti args <$> k r
  TermLiteral lit r -> TermLiteral lit <$> k r
  TermPrimitive prim r -> TermPrimitive prim <$> k r
  TermAssert tm1 tm2 r -> TermAssert tm1 tm2 <$> k r
  TermLet x tm1 tm2 r -> TermLet x tm1 tm2 <$> k r

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

-- TODO: SHOULDNT need this
-- substTerm :: TermId -> Term r -> Term r -> Term r
-- substTerm tmId tm' term = case term of
--   TermLiteral {} -> term
--   TermSymbol {} -> term
--   TermNamed tmId' _ | tmId' == tmId -> tm'
--   TermNamed {} -> term
--   TermPrimitive prim r -> flip TermPrimitive r case prim of
--     PrimitiveTry te -> PrimitiveTry (go te)
--     PrimitiveTuple tes -> PrimitiveTuple (go <$> tes)
--     PrimitiveArray tes -> PrimitiveArray (go <$> tes)
--     PrimitiveIf te te' te3 -> PrimitiveIf (go te) (go te') (go te3)
--     PrimitiveAnd te te' -> PrimitiveAnd (go te) (go te')
--     PrimitiveOr te te' -> PrimitiveOr (go te) (go te')
--     PrimitiveNot te -> PrimitiveNot (go te)
--     PrimitiveEq te te' -> PrimitiveEq (go te) (go te')
--     PrimitiveAdd te te' -> PrimitiveAdd (go te) (go te')
--   TermAssert te te' r -> TermAssert (go te) (go te') r
--   where
--     go = substTerm tmId tm'
