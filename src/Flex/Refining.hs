module Flex.Refining where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq
import Control.Exception
import Control.Monad (void, when)
import Data.Bifunctor (second)
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import Data.Typeable
import Flex.Syntax (Id, Literal, ModuleId)
import qualified Flex.Syntax as Syn
import GHC.Generics
import qualified Language.Fixpoint.Horn.Solve as HS
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Misc as F
import qualified Language.Fixpoint.Misc as Misc
import qualified Language.Fixpoint.Parse as FP
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.Config as FC
import qualified Language.Fixpoint.Utils.Files as Files
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

data BaseType_ r
  = TypeAtomic r Atomic
  | TypeTuple r ![Type_ r]
  | TypeArray r !(Type_ r)
  | TypeOptional r (Type_ r)
  -- TODO:
  --   | TypeStructure Structure r
  --   | TypeEnumerated Enumerated r
  --   | TypeVariant Variant r
  --   | TypeNewtype NewType_ r
  deriving (Eq, Show)

data Atomic
  = AtomicInt
  | AtomicUInt
  | AtomicFloat
  | AtomicBit
  | AtomicChar
  deriving (Eq, Show)

-- | FunType
type FunType = FunType_ F.Reft

data FunType_ r
  = FunType ![(F.Symbol, BaseType_ r)] !(BaseType_ r)
  deriving (Eq, Show)

-- | Subable (Subtypeable)
instance F.Subable r => F.Subable (Type_ r) -- TODO

instance F.Subable r => F.Subable (BaseType_ r) -- TODO

instance F.Subable r => F.Subable (FunType_ r) -- TODO

-- | Substitution
--
-- TODO: what exactly does this do?
subst :: F.Subable a => a -> F.Symbol -> F.Symbol -> a
subst = undefined -- TODO

-- | Embedding
--
-- Embed a term as a LF expression
embedTerm :: Term -> F.Expr
embedTerm = undefined

-- | RefineError
data RefineError = RefineError String

-- | Constraints
--
-- TODO: desc
type Cstr = H.Cstr RefineError

trivialCstr :: Cstr
trivialCstr = H.CAnd []

andCstr :: Cstr -> Cstr -> Cstr
andCstr cstr1 cstr2 = andCstrs [cstr1, cstr2]

andCstrs :: [Cstr] -> Cstr
andCstrs = H.CAnd

forallCstr :: F.Symbol -> BaseType -> Cstr -> Cstr
forallCstr x ty cstr = case sortPred x ty of
  Just (srt, prd) -> H.All (H.Bind x srt prd (RefineError "forallCstr")) cstr
  _ -> cstr

-- subtyping constraint (??)
headCstr :: F.Expr -> Cstr
headCstr e = H.Head (H.Reft e) (RefineError "Subtype error")

reftSymbol :: F.Reft -> F.Symbol
reftSymbol (F.Reft (x, _)) = x

reftExpr :: F.Reft -> F.Expr
reftExpr (F.Reft (_, e)) = e

sortPred :: F.Symbol -> BaseType -> Maybe (F.Sort, H.Pred)
sortPred x = \case
  TypeAtomic r atom ->
    Just
      ( case atom of
          AtomicInt -> F.intSort
          AtomicUInt -> F.intSort
          AtomicFloat -> F.realSort
          AtomicBit -> F.boolSort
          AtomicChar -> F.charSort,
        mkReft r
      )
  TypeArray _r _ -> Just undefined -- TODO
  TypeOptional _r _ -> Just undefined -- TODO
  where
    mkReft r = H.Reft (subst (reftExpr r) (reftSymbol r) x)

--   IntType (F.Reft (v, p)) -> Just (F.intSort, H.Reft (subst p v x))
--   _ -> Nothing

-- | Term
--
-- TODO: desc
data Term
  = TermLiteral !Literal
  | TermNamed !F.Symbol
  | TermTuple ![Term]
  | TermArray ![Term]
  | TermBlock !Block
  | -- TODO:
    --   | TermStructure Structure (Map.Map Text Term)
    --   | TermMember Type -- Term Text
    --   | TermConstructor Type
    TermApplication !F.Symbol [Term]
  | TermAscribe !Term !BaseType
  --   | TermMatch Type
  deriving (Eq, Show)

type Block = ([Statement], Term)

data Statement
  = StatementLet !F.Symbol !(Maybe BaseType) !Term
  | StatementAssert !Term !BaseType
  deriving (Eq, Show)

-- | Label
type Label = F.SrcSpan

class HasLabel a where
  getLabel :: a -> Label

instance HasLabel Term where
  getLabel _ = F.dummySpan

-- | Constraint Generation monad
type CG a = Either [RefineError] a

throwCG :: [RefineError] -> CG a
throwCG = Left

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

-- | Checking (check)
check :: Env -> Term -> BaseType -> CG Cstr
check env tm tyExp = do
  (cstr, tyInf) <- synth env tm
  cstr' <- checkSubtype tyInf tyExp
  return $ andCstrs [cstr, cstr']

checkBlock :: Env -> Block -> BaseType -> CG Cstr
checkBlock env ([], tm) tyExp = check env tm tyExp
checkBlock env (stmt : stmts, tm) tyExp = case stmt of
  StatementLet x mb_sigExp imp -> do
    (cstr, sigInf) <- synth env imp
    (cstr, sig) <-
      case mb_sigExp of
        -- if there is an annotated signature, then check that the
        -- implementation satisfies it i.e. the inferred signature is a subtype
        -- of the annotated (expected) signature
        Just sigExp -> checkSubtype sigInf sigExp <&> (,sigExp)
        -- otherwise, just use the inferred signature
        Nothing -> return (andCstr cstr trivialCstr, sigInf)
    -- universally quantify over the introduced value with the appropriate
    -- signature
    andCstr cstr . forallCstr x sig
      <$> checkBlock (extendEnv x (TypeBaseType sig) env) (stmts, tm) tyExp
  StatementAssert tm' ty' ->
    liftA2
      andCstr
      -- check that the term has the annotated type
      (check env tm' ty')
      (checkBlock env (stmts, tm) tyExp)

-- | Synthesizing (synth)
synth :: Env -> Term -> CG (Cstr, BaseType)
synth _env (TermLiteral lit) = (trivialCstr,) <$> synthLiteral lit
synth env (TermNamed x) =
  (trivialCstr,)
    <$> ( lookupEnv x env
            >>= ( \case
                    TypeBaseType ty -> return ty
                    TypeFunType funTy ->
                      throwCG
                        [ RefineError . concat $
                            [ "type synthesis error;",
                              "expected the variable",
                              " '" <> show x <> "' ",
                              "to have be a constant, but it's actually a function of type",
                              " '" <> show funTy <> "'"
                            ]
                        ]
                )
        )
synth env (TermTuple tes) = undefined -- TODO: how to refine tuples? look at SPRITE
synth env (TermArray tes) = undefined -- TODO: how to refine arrays? look at SPRITE
synth env (TermBlock x0) = throwCG [RefineError "should never synthesize a TermBlock; should only ever check"]
synth env (TermApplication x args) = do
  -- get the function type
  -- check the args with their corresponding param types
  -- substitute the arguments for their values in the output type
  undefined
synth env (TermAscribe tm ty) = do
  cstr <- check env tm ty
  return (cstr, ty)

synthLiteral :: Literal -> CG BaseType
synthLiteral = error "synthLiteral"

synthFun :: Env -> Id -> CG FunType
synthFun = error "synthFun"

-- | Subtype checking (checkSubtype)
--
-- Check that one type is a subtype of another type (taking refinements into
-- account).
checkSubtype :: BaseType -> BaseType -> CG Cstr
--    forall x : T, p x ==> (p y)[y := x]
--  ----------------------------------------------
--    {x : T | p x} <: {y : T | p y}
checkSubtype ty1@(TypeAtomic r1 atom1) (TypeAtomic r2 atom2)
  | atom1 == atom2 =
      return $
        forallCstr
          (reftSymbol r1)
          ty1
          (headCstr (subst (reftExpr r2) (reftSymbol r2) (reftSymbol r1)))
checkSubtype (TypeArray _r1 _ty1) (TypeArray _r2 _ty2) =
  error "checkSubtype TypeArray"
checkSubtype (TypeOptional _r1 _ty1) (TypeOptional _t2 _ty2) =
  error "checkSubtype TypeOptional"
checkSubtype ty1 ty2 =
  throwCG
    [ RefineError $
        "subtyping error; the type"
          <> (" '" <> show ty1 <> "' ")
          <> "cannot be checked to be a subtype of the type"
          <> (" '" <> show ty2 <> "' ")
    ]
