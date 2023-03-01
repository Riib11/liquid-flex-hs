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
    TypeBaseType baseTy -> case baseTy of
      TypeAtomic r _ -> F.syms r
      TypeTuple _r _ -> error "TODO: syms"
      TypeArray _r _ -> error "TODO: syms"
      TypeOptional _r _ -> error "TODO: syms"
    TypeFunType (FunType params outTy) ->
      concatMap
        (F.syms . snd)
        params
        <> F.syms outTy

  substa f = \case
    TypeBaseType baseTy -> case baseTy of
      TypeAtomic r atomic -> TypeBaseType $ TypeAtomic (F.substa f r) atomic
      TypeTuple _r _ -> error "TODO: substa"
      TypeArray _r _ -> error "TODO: substa"
      TypeOptional _r _ -> error "TODO: substa"
    TypeFunType (FunType params outTy) ->
      TypeFunType $
        FunType
          (second (F.substa f) <$> params)
          (F.substa f outTy)
  substf f = \case
    TypeBaseType baseTy -> case baseTy of
      TypeAtomic r atomic -> TypeBaseType $ TypeAtomic (F.substf f r) atomic
      TypeTuple _r _ -> error "TODO: substa"
      TypeArray _r _ -> error "TODO: substa"
      TypeOptional _r _ -> error "TODO: substa"
    TypeFunType (FunType params outTy) ->
      TypeFunType $
        FunType
          (second (F.substf f) <$> params)
          (F.substf f outTy)
  subst f = \case
    TypeBaseType baseTy -> case baseTy of
      TypeAtomic r atomic -> TypeBaseType $ TypeAtomic (F.subst f r) atomic
      TypeTuple _r _ -> error "TODO: substa"
      TypeArray _r _ -> error "TODO: substa"
      TypeOptional _r _ -> error "TODO: substa"
    TypeFunType (FunType params outTy) ->
      TypeFunType $
        FunType
          (second (F.subst f) <$> params)
          (F.subst f outTy)

instance F.Subable r => F.Subable (BaseType_ r) -- TODO

instance F.Subable r => F.Subable (FunType_ r) -- TODO

-- | Substitution
--
-- substitute `x` for `y` in `thing` via Subable
subst :: F.Subable a => a -> F.Symbol -> F.Symbol -> a
subst thing x y = F.subst (F.mkSubst [(x, F.expr y)]) thing

-- subst' :: F.Subable a => a -> F.Symbol -> Term -> a
-- subst' thing x y = F.subst sigma thing
--   where
--     sigma = F.mkSubst [(x, varExpr y)]

-- varExpr :: F.Symbol -> F.Expr
-- varExpr = F.expr

-- litExpr :: Literal -> F.Expr
-- litExpr = error "TODO"

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
  TypeTuple _r _ -> Just (error "TODO: how to refine tuples")
  TypeArray _r _ -> Just (error "TODO: how to refine arrays")
  TypeOptional _r _ -> Just (error "TODO: how to refine options")
  where
    mkReft r = H.Reft (subst (reftExpr r) (reftSymbol r) x)

-- | Term
--
-- TODO: desc
data Term
  = TermLiteral !Literal
  | TermVar !F.Symbol
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
synth env (TermVar x) = (trivialCstr,) <$> synthCon env x
synth env (TermTuple tes) = error "TODO: how to refine tuples? look at SPRITE"
synth env (TermArray tes) = error "TODO: how to refine arrays? look at SPRITE"
synth env (TermBlock x0) = throwCG [RefineError "should never synthesize a TermBlock; should only ever check"]
synth env (TermApplication x args) = do
  -- get the function type
  FunType params tyOut <- synthFun env x
  -- check the args with their corresponding param types
  cstr <-
    andCstrs
      <$> mapM
        -- since function types are _simple_, don't need to update environment
        -- with values of arguments
        (\(tm, (_x, ty)) -> check env tm ty)
        (args `zip` params)
  -- since function types are _simple_, don't need to substitute parameters for
  -- their argument values in the output type
  return (cstr, tyOut)
synth env (TermAscribe tm ty) = do
  cstr <- check env tm ty
  return (cstr, ty)

synthLiteral :: Literal -> CG BaseType
synthLiteral = \case
  Syn.LiteralInteger n ->
    return $ TypeAtomic (F.exprReft (F.expr n)) AtomicInt
  Syn.LiteralFloat x ->
    -- TODO: probably want to use something like sized bitvectors? but need to
    -- keep track of floating-point math accuracy, so is more complicated
    error "TODO: embedding floats in LF"
  Syn.LiteralBit b -> return $ TypeAtomic (F.exprReft (F.expr i)) AtomicBit
    where
      i :: Int
      i = if b then 1 else 0
  Syn.LiteralChar c ->
    return $ TypeAtomic (F.exprReft (F.expr (pack [c]))) AtomicChar
  Syn.LiteralString txt ->
    return $ TypeAtomic (F.exprReft (F.expr txt)) AtomicString

-- error "TODO: synthLiteral LiteralString"

synthCon :: Env -> F.Symbol -> CG BaseType
synthCon env x =
  lookupEnv x env
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

synthFun :: Env -> F.Symbol -> CG FunType
synthFun env x = case F.lookupSEnv x env of
  Just (TypeFunType funTy) -> return funTy
  Just _ -> throwCG [RefineError $ "expected to be a function id: " <> show x]
  Nothing -> throwCG [RefineError $ "unknown function id: " <> show x]

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
