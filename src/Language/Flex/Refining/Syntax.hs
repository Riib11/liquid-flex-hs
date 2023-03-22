{-# HLINT ignore "Use camelCase" #-}
module Language.Flex.Refining.Syntax where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq
import Control.Exception
import Control.Monad (foldM, void, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Bifunctor (Bifunctor (bimap), second)
import Data.Foldable (toList)
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
import qualified Language.Fixpoint.Types.PrettyPrint as P
import qualified Language.Fixpoint.Utils.Files as Files
import qualified Language.Flex.FlexBug as FlexBug
import Language.Flex.FlexM (pprintInline)
import qualified Language.Flex.FlexM as FlexM
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

type TypeReft = Type F.Reft

-- TODO: handle more advanced types

-- | TypeArray
-- | TypeOptional
-- | TypeStructure
-- | TypeEnumerated
-- | TypeVariant
-- | TypeNewtype
data Type r
  = TypeAtomic {typeAtomic :: AtomicType, typeAnn :: r}
  | TypeTuple {typeTupleComponents :: !(Type r, Type r), typeAnn :: r}
  deriving
    (Eq, Show, Functor, Foldable, Traversable)

instance Pretty (Type F.Reft) where
  pPrint = \case
    TypeAtomic at r -> case at of
      TypeInt -> go "int" r
      TypeFloat -> go "float" r
      TypeBit -> go "bit" r
      TypeChar -> go "char" r
      TypeString -> go "string" r
    TypeTuple (ty1, ty2) r -> go (parens $ (pPrint ty1 <> ",") <+> pPrint ty2) r
    where
      go doc r = braces $ pprintInline (F.reftBind r) <+> ":" <+> doc <+> "|" <+> pprintInline (F.reftPred r)

voidTypeR :: Type r -> Type ()
voidTypeR = void

data AtomicType
  = TypeInt
  | TypeFloat
  | TypeBit
  | TypeChar
  | TypeString
  deriving (Eq, Show)

mapM_typeAnn :: Functor f => (r -> f r) -> Type r -> f (Type r)
mapM_typeAnn k ty = do
  ann <- k (typeAnn ty)
  return ty {typeAnn = ann}

typeEqTrue :: TypeReft
typeEqTrue = typeBit F.trueReft

typeEqFalse :: TypeReft
typeEqFalse = typeBit F.falseReft

typeInt :: r -> Type r
typeInt = TypeAtomic TypeInt

typeBit :: r -> Type r
typeBit = TypeAtomic TypeBit

typeChar :: F.Reft -> TypeReft
typeChar = TypeAtomic TypeChar

typeString :: F.Reft -> TypeReft
typeString = TypeAtomic TypeString

primitiveLocated :: P.PPrint a => a -> F.Located a
primitiveLocated a = F.Loc {loc = l, locE = l, val = a}
  where
    l = primitiveSourcePos (F.symbol $ "<primitive:" ++ render (pprintInline a) ++ ">")

primitiveSourcePos :: F.Symbol -> F.SourcePos
primitiveSourcePos label =
  F.SourcePos
    { sourceName = render (pprintInline label),
      sourceLine = F.mkPos 1,
      sourceColumn = F.mkPos 1
    }

-- ** FunctionType

-- Liquid Flex's function types are simple in that the they cannot express
-- dependency of one type's refinement on a preceeding parameter's value.
type FunctionTypeReft = FunctionType F.Reft

data FunctionType r = FunctionType
  { functionTypeParameters :: ![Type r],
    functionTypeOutput :: !(Type r)
  }
  deriving (Eq, Show)

-- | Subable (Subtypeable)
instance F.Subable r => F.Subable (Type r) where
  syms = foldMap F.syms
  substa f = fmap (F.substa f)
  substf f = fmap (F.substf f)
  subst f = fmap (F.subst f)

instance F.Subable r => F.Subable (FunctionType r) where
  syms (FunctionType _params outTy) = F.syms outTy
  substa f (FunctionType params outTy) = FunctionType params (F.substa f outTy)
  substf f (FunctionType params outTy) = FunctionType params (F.substf f outTy)
  subst f (FunctionType params outTy) = FunctionType params (F.subst f outTy)

-- ** Structure

data Structure = Structure
  { structureId :: TypeId,
    structureIsMessage :: Bool,
    structureMaybeExtensionId :: Maybe TypeId,
    structureFields :: [(FieldId, TypeReft)],
    structureRefinement :: F.Expr
  }
  deriving (Eq, Show)

-- ** Variant

data Variant = Variant
  { variantId :: TypeId,
    variantConstructors :: [(TermId, Maybe [TypeReft])]
  }
  deriving (Eq, Show)

-- ** Enum

data Enum = Enum
  { enumId :: TypeId,
    enumType :: TypeReft,
    enumConstructors :: [(TermId, Literal)]
  }
  deriving (Eq, Show)

-- ** Function

data Function r = Function
  { functionId :: TermId,
    functionIsTransform :: Bool,
    functionParameters :: [(TermId, TypeReft)],
    functionOutput :: TypeReft,
    functionBody :: Term r
  }
  deriving (Eq, Show)

-- ** Constant

data Constant r = Constant
  { constantId :: TermId,
    constantBody :: Term r,
    constantType :: TypeReft
  }
  deriving (Eq, Show)

-- ** Primitive

data Primitive r
  = PrimitiveTry (Term r)
  | PrimitiveTuple (Term r, Term r)
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
    PrimitiveTuple (te1, te2) -> parens $ (pPrint te1 <> ",") <+> pPrint te2
    PrimitiveArray tes -> brackets $ hsep $ punctuate comma $ pPrint <$> tes
    PrimitiveIf te te' te'' -> parens $ text "if" <+> pPrint te <+> text "then" <+> pPrint te' <+> text "else" <+> pPrint te''
    PrimitiveAnd te te' -> parens $ pPrint te <+> text "&&" <+> pPrint te'
    PrimitiveOr te te' -> parens $ pPrint te <+> text "||" <+> pPrint te'
    PrimitiveNot te -> parens $ text "!" <+> pPrint te
    PrimitiveEq te te' -> parens $ pPrint te <+> "==" <+> pPrint te'
    PrimitiveAdd te te' -> parens $ pPrint te <+> "+" <+> pPrint te'

-- ** Term

-- TODO: structure, member, construct enum, construct variant, match
data Term r
  = TermNeutral {termId' :: !Id', termArgs :: [Term r], termAnn :: r}
  | TermLiteral {termLiteral :: !Literal, termAnn :: r}
  | TermPrimitive {termPrimitive :: !(Primitive r), termAnn :: r}
  | TermLet {termId' :: !Id', termTerm :: !(Term r), termBody :: !(Term r), termAnn :: r}
  | TermAssert {termTerm :: !(Term r), termBody :: !(Term r), termAnn :: r}
  deriving (Eq, Show, Functor)

instance Pretty (Term r) where
  pPrint = \case
    TermNeutral id' tes _r
      | null tes -> pPrint id'
      | otherwise -> pPrint id' <> parens (hsep $ punctuate comma $ pPrint <$> tes)
    TermLiteral lit _r -> pPrint lit
    TermPrimitive prim _r -> pPrint prim
    TermLet id' te te' _r -> (text "let" <+> pPrint id' <+> text "=" <+> pPrint te <+> ";") $$ pPrint te'
    TermAssert te te' _r -> (text "assert" <+> pPrint te <+> ";") $$ pPrint te'

data Id' = Id'
  { id'Symbol :: !F.Symbol,
    id'MaybeTermId :: !(Maybe TermId)
  }
  deriving (Eq, Ord, Show)

instance Pretty Id' where
  pPrint (Id' sym _m_ti) = pprintInline sym

termVar :: Id' -> r -> Term r
termVar id' = TermNeutral id' []

fromSymbolToTerm :: F.Symbol -> r -> Term r
fromSymbolToTerm sym = termVar (fromSymbolToId' sym)

fromSymbolToId' :: F.Symbol -> Id'
fromSymbolToId' id'Symbol = Id' {id'Symbol, id'MaybeTermId = Nothing}

mapM_termAnn :: Monad m => (r -> m r) -> Term r -> m (Term r)
mapM_termAnn k tm = do
  ann <- k (termAnn tm)
  return tm {termAnn = ann}

-- ** Substitution

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
