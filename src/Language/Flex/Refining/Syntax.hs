{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Flex.Refining.Syntax where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq
import Control.Exception
import Control.Monad (foldM, void, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Bifunctor (Bifunctor (bimap), second)
import Data.Foldable (toList)
import Data.Functor
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import Data.Typeable
import Data.Void (Void)
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
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Logic (replaceSym)
import Language.Flex.Syntax (FieldId, Literal (..), TermId, TypeId)
import qualified Language.Flex.Syntax as Base
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Text.Printf (printf)
import Utility

-- ** Liquid-Fixpoint relevant types

-- | Query
--
-- The query includes all of the constraints gathered up during
-- checking/synthesizing.
type Query = H.Query RefiningError

-- | Result
--
-- Result received back after submitting query
type Result = F.FixResult RefiningError

-- | Constraints
--
-- In Liquid Fixpoint, `H.Cstr` has the following form:
--
--    data Cstr a
--      = Head  !Pred !a                  -- ^ p
--      | CAnd  ![Cstr a]                 -- ^ c1 /\ ... /\ cn
--      | All   !(Bind a)  !(Cstr a)      -- ^ \all x:t. p => c
--      | Any   !(Bind a)  !(Cstr a)      -- ^ \exi x:t. p /\ c or is it \exi x:t. p => c?
--      deriving (Data, Typeable, Generic, Functor, Eq)
type Cstr = H.Cstr RefiningError

type Bind = H.Bind RefiningError

-- ** RefiningError

newtype RefiningError = RefiningError Doc
  deriving (Eq, Show, Generic)

instance NFData RefiningError

instance Pretty RefiningError where
  pPrint (RefiningError msg) = msg

instance F.Fixpoint RefiningError where
  toFix = text . renderInline . pPrint

instance F.Loc RefiningError where
  srcSpan _ = F.dummySpan

instance F.PPrint RefiningError where
  pprintTidy _ = text . renderInline . pPrint

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

type TypeReft = Type QReft

data QReft = QReft
  { qreftQuants :: [Quant],
    -- reftSort :: F.Sort,
    qreftReft :: F.Reft
  }
  deriving (Eq, Show)

instance Semigroup QReft where
  -- reft1@(QReft qs1 srt1 r1) <> reft2@(QReft qs2 srt2 r2 )
  --   | srt1 == srt2 = QReft (qs1 <> qs2) srt1 (r1 <> r2)
  --   | otherwise = error . render $ "BUG: attempted to append two @QReft@'s that have different sorts: " <> pPrint reft1 <> " <> " <> pPrint reft2
  QReft qs1 r1 <> QReft qs2 r2 = QReft (qs1 <> qs2) (r1 <> r2)

instance Monoid QReft where
  mempty = QReft mempty mempty

instance F.Subable QReft where
  syms :: QReft -> [F.Symbol]
  syms QReft {..} =
    Set.toList $
      Set.fromList (concatMap F.syms qreftQuants <> F.syms qreftReft)
        -- subtract out all the symbol that are quantified by qrefQuants
        Set.\\ Set.fromList (H.bSym . quantBind <$> qreftQuants)
  substa :: (F.Symbol -> F.Symbol) -> QReft -> QReft
  substa f qr@QReft {..} = qr {qreftQuants = F.substa f <$> qreftQuants, qreftReft = F.substa f qreftReft}
  substf :: (F.Symbol -> F.Expr) -> QReft -> QReft
  substf f qr@QReft {..} = qr {qreftQuants = F.substf f <$> qreftQuants, qreftReft = F.substf f qreftReft}
  subst :: F.Subst -> QReft -> QReft
  subst sub qr@QReft {..} = qr {qreftQuants = F.subst sub <$> qreftQuants, qreftReft = F.subst sub qreftReft}

instance Pretty QReft where
  pPrint QReft {..} = hcat ((<+> ".") . pPrint <$> qreftQuants) <+> pprintInline qreftReft

data Quant
  = QuantForall {quantBind :: Bind}
  | QuantExists {quantBind :: Bind}
  deriving (Eq, Show)

instance F.Subable Quant where
  syms :: Quant -> [F.Symbol]
  syms = F.syms . quantBind
  substa :: (F.Symbol -> F.Symbol) -> Quant -> Quant
  substa f q = q {quantBind = F.substa f (quantBind q)}
  substf :: (F.Symbol -> F.Expr) -> Quant -> Quant
  substf f q = q {quantBind = F.substf f (quantBind q)}
  subst :: F.Subst -> Quant -> Quant
  subst sub q = q {quantBind = F.subst sub (quantBind q)}

instance Pretty Quant where
  pPrint = \case
    QuantForall bnd -> F.pprint bnd
    QuantExists bnd -> F.pprint bnd

quantCstr :: Quant -> Cstr -> Cstr
quantCstr = \case
  QuantForall bnd -> H.All bnd
  QuantExists bnd -> H.Any bnd

qreftBind :: QReft -> F.Symbol
qreftBind = F.reftBind . qreftReft

qreftPred :: QReft -> F.Pred
qreftPred = F.reftPred . qreftReft

setQReftBind :: F.Symbol -> QReft -> QReft
setQReftBind x' qr@QReft {..} =
  QReft
    { qreftQuants = F.substa (replaceSym x x') <$> qreftQuants,
      qreftReft = F.substa (replaceSym x x') qreftReft
    }
  where
    x = qreftBind qr

setReftBind :: F.Symbol -> F.Reft -> F.Reft
setReftBind x' r = F.substa (replaceSym x x') r
  where
    x = F.reftBind r

fromReft :: F.Reft -> QReft
fromReft qreftReft = QReft {qreftQuants = mempty, qreftReft}

-- TODO: handle more advanced types
-- TODO: can structure and newtype be merged for the refinement phase?

-- | TypeArray
-- | TypeOptional
-- | TypeStructure
-- | TypeEnumerated
-- | TypeVariant
-- | TypeNewtype
data Type r
  = TypeAtomic {typeAtomic :: AtomicType, typeAnn :: r}
  | TypeTuple {typeTupleComponents :: !(Type r, Type r), typeAnn :: r}
  | TypeStructure {typeStructure :: Base.Structure, typeAnn :: r}
  deriving
    (Eq, Show, Functor, Foldable, Traversable)

instance Pretty TypeReft where
  pPrint = \case
    TypeAtomic at r -> case at of
      TypeInt -> go "int" r
      TypeFloat -> go "float" r
      TypeBit -> go "bit" r
      TypeChar -> go "char" r
      TypeString -> go "string" r
    TypeTuple (ty1, ty2) r -> go (parens $ (pPrint ty1 <> ",") <+> pPrint ty2) r
    TypeStructure Base.Structure {..} r -> go (pPrint structureId) r
    where
      go doc r@(QReft {..}) = braces $ pprintInline (F.reftBind qreftReft) <+> ":" <+> doc <+> "|" <+> pPrint r

instance Pretty (Type ()) where
  pPrint = \case
    TypeAtomic at _r -> case at of
      TypeInt -> "int"
      TypeFloat -> "float"
      TypeBit -> "bit"
      TypeChar -> "char"
      TypeString -> "string"
    TypeTuple (ty1, ty2) _r -> (parens $ (pPrint ty1 <> ",") <+> pPrint ty2)
    TypeStructure Base.Structure {..} _r -> (pPrint structureId)

data AtomicType
  = TypeInt
  | TypeFloat
  | TypeBit
  | TypeChar
  | TypeString
  deriving (Eq, Show)

-- TODO: is it ok to use F.PTrue as an expression here? or is there a different
-- encoding of booleans as expression, e.g. as ints or something?
typeEqTrue :: TypeReft
typeEqTrue = typeBit $ QReft [] (F.exprReft F.PTrue)

typeEqFalse :: TypeReft
typeEqFalse = typeBit $ QReft [] (F.exprReft F.PFalse)

typeInt :: r -> Type r
typeInt = TypeAtomic TypeInt

typeBit :: r -> Type r
typeBit = TypeAtomic TypeBit

typeChar :: QReft -> TypeReft
typeChar = TypeAtomic TypeChar

typeString :: QReft -> TypeReft
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
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance PrettyTermAnn r => Pretty (Primitive r) where
  pPrint = \case
    PrimitiveTry te -> parens $ text "try" <+> pPrint te
    PrimitiveTuple (te1, te2) -> parens $ (pPrint te1 <> ",") <+> pPrint te2
    PrimitiveArray tes -> brackets $ hsep $ punctuate (comma <> space) $ pPrint <$> tes
    PrimitiveIf te te' te'' -> parens $ text "if" <+> pPrint te <+> text "then" <+> pPrint te' <+> text "else" <+> pPrint te''
    PrimitiveAnd te te' -> parens $ pPrint te <+> text "&&" <+> pPrint te'
    PrimitiveOr te te' -> parens $ pPrint te <+> text "||" <+> pPrint te'
    PrimitiveNot te -> parens $ text "!" <+> pPrint te
    PrimitiveEq te te' -> parens $ pPrint te <+> "==" <+> pPrint te'
    PrimitiveAdd te te' -> parens $ pPrint te <+> "+" <+> pPrint te'

-- ** Term

-- TODO: construct enum, construct variant, match
data Term r
  = TermNeutral {termSymId :: !SymId, termArgs :: [Term r], termAnn :: r}
  | TermLiteral {termLiteral :: !Literal, termAnn :: r}
  | TermPrimitive {termPrimitive :: !(Primitive r), termAnn :: r}
  | TermLet {termSymId :: !SymId, termTerm :: !(Term r), termBody :: !(Term r), termAnn :: r}
  | TermAssert {termTerm :: !(Term r), termBody :: !(Term r), termAnn :: r}
  | TermStructure {termStructure :: !Base.Structure, termFields :: ![(FieldId, Term r)], termAnn :: r}
  | TermMember {termStructure :: !Base.Structure, termTerm :: !(Term r), termFieldId :: !Base.FieldId, termAnn :: r}
  deriving (Eq, Show, Functor, Foldable, Traversable)

class PrettyTermAnn r where
  pPrintAnn :: r -> Doc -> Doc

instance PrettyTermAnn () where
  pPrintAnn () = id

instance PrettyTermAnn Base.Type where
  pPrintAnn ty = (<+> (":" <+> pPrint ty))

instance PrettyTermAnn (Type ()) where
  pPrintAnn ty = (<+> (":" <+> pPrint ty))

instance PrettyTermAnn TypeReft where
  pPrintAnn ty = (<+> (":" <+> pPrint ty))

instance PrettyTermAnn r => Pretty (Term r) where
  pPrint term = case term of
    TermNeutral symId tes r
      | null tes -> pPrintAnn r $ pPrint symId
      | otherwise -> pPrintAnn r $ pPrint symId <> parens (hcat $ punctuate (comma <> space) $ pPrint <$> tes)
    TermLiteral lit r -> parens $ pPrintAnn r $ pPrint lit
    TermPrimitive prim r -> pPrintAnn r $ pPrint prim
    TermLet symId te te' _r -> (text "let" <+> pPrint symId <+> equals <+> pPrint te <+> semi) $$ pPrint te'
    TermAssert te te' _r -> (text "assert" <+> pPrint te <+> ";") $$ pPrint te'
    TermStructure {..} -> parens $ pPrintAnn termAnn $ pPrint (Base.structureId termStructure) <> braces (hcat (punctuate (comma <> space) (termFields <&> \(fieldId, tm) -> pPrint fieldId <+> "=" <+> pPrint tm)))
    TermMember {..} -> parens $ pPrintAnn termAnn $ pPrint termTerm <> "#" <> pPrint termFieldId

data SymId = SymId
  { symIdSymbol :: !F.Symbol,
    symIdMaybeTermId :: !(Maybe TermId)
  }
  deriving (Eq, Ord, Show)

instance Pretty SymId where
  pPrint (SymId sym _m_ti) = pprintInline sym

varTerm :: SymId -> r -> Term r
varTerm symId = TermNeutral symId []

fromSymbolToTerm :: F.Symbol -> r -> Term r
fromSymbolToTerm sym = varTerm (fromSymbolToSymId sym)

fromSymbolToSymId :: F.Symbol -> SymId
fromSymbolToSymId symIdSymbol = SymId {symIdSymbol, symIdMaybeTermId = Nothing}

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
--   TermNamed tmSymId _ | tmSymId == tmId -> tm'
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
