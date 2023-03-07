module Flex.Refining.Check where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq
import Control.Exception
import Control.Monad (foldM, void, when, zipWithM)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Bifunctor (second)
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import Data.Typeable
import Flex.Flex (refineError)
import Flex.Refining.Common
import Flex.Refining.Constraint
import Flex.Refining.Embedding
import Flex.Refining.Syntax
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

-- | Checking
--
-- The type needs to be re-synthesized even though the term is already annotated
-- because it could result in constraints and aslo the annotated type is
-- unrefined.
checkTerm :: Env -> Term -> BaseType -> Refining Cstr
checkTerm env tm tyExp = do
  (cstr, tyInf) <- synth env tm
  cstr' <- checkSubtype tyInf tyExp
  return $ andCstrs [cstr, cstr']

checkBlock :: Env -> Block -> BaseType -> Refining Cstr
checkBlock env ([], tm) tyExp = checkTerm env tm tyExp
checkBlock env (stmt : stmts, tm) tyExp = case stmt of
  StatementLet x imp -> do
    (cstr, sig) <- synth env imp
    -- universally quantify over the introduced value with the appropriate
    -- signature
    andCstr cstr . forallCstr x sig
      <$> checkBlock (extendEnv x (TypeBaseType sig) env) (stmts, tm) tyExp
  StatementAssert tm' ->
    liftA2
      andCstr
      -- checkTerm that the term has the `true` refinement type
      (checkTerm env tm' trueBaseType)
      (checkBlock env (stmts, tm) tyExp)

-- | Synthesizing (synth)
synth :: Env -> Term -> Refining (Cstr, BaseType)
synth env (Term ptm ty) = case ptm of
  TermLiteral lit -> (trivialCstr,) <$> synthLiteral lit ty
  TermVar x -> (trivialCstr,) <$> synthCon env x
  TermBlock _ -> throwError . refineError $ "should never synthesize a TermBlock; should only ever check"
  TermApp (AppPrimFun pf) args -> synthAppPrimFun env pf args
  TermApp (AppVar x) args -> do
    -- synth the arg types
    tyArgs <- (snd <$>) <$> (synth env `traverse` args)
    -- synth the fun type
    FunType params tyOut <- synthFun env x tyArgs
    -- checkTerm the args with their corresponding param types; note that since
    -- function types are _simple_, don't need to update environment with values
    -- of arguments
    cstr <- andCstrs <$> zipWithM (checkTerm env) args params
    -- since function types are _simple_, don't need to substitute parameters
    -- for their argument values in the output type
    return (cstr, tyOut)

reftTerm :: Term -> Refining F.Reft
reftTerm tm = F.exprReft <$> embedTerm tm

reftPreterm :: Preterm -> Refining F.Reft
reftPreterm tm = F.exprReft <$> embedPreterm tm

synthAppPrimFun :: Env -> Syn.PrimFun -> [Term] -> Refining (Cstr, BaseType)
synthAppPrimFun env Syn.PrimFunEq [tm1, tm2] = do
  -- synth type of first arg
  (cstr, ty1) <- synth env tm1
  -- check that second arg has same (unrefined) type
  cstr <- andCstr cstr <$> checkTerm env tm2 (unrefineBaseType ty1)
  r <- reftPreterm $ TermApp (AppPrimFun Syn.PrimFunEq) [tm1, tm2]
  return (cstr, TypeAtomic r AtomicBit)
synthAppPrimFun env Syn.PrimFunAnd [tm1, tm2] = do
  cstr <- andCstrs <$> traverse (\tm -> checkTerm env tm (TypeAtomic F.trueReft AtomicBit)) [tm1, tm2]
  r <- reftPreterm $ TermApp (AppPrimFun Syn.PrimFunAnd) [tm1, tm2]
  return (cstr, TypeAtomic r AtomicBit)
synthAppPrimFun env Syn.PrimFunOr [tm1, tm2] = do
  cstr <- andCstrs <$> traverse (\tm -> checkTerm env tm (TypeAtomic F.trueReft AtomicBit)) [tm1, tm2]
  r <- reftPreterm $ TermApp (AppPrimFun Syn.PrimFunOr) [tm1, tm2]
  return (cstr, TypeAtomic r AtomicBit)
synthAppPrimFun env Syn.PrimFunNot [tm] = do
  cstr <- checkTerm env tm (TypeAtomic F.trueReft AtomicBit)
  r <- reftPreterm $ TermApp (AppPrimFun Syn.PrimFunNot) [tm]
  return (cstr, TypeAtomic r AtomicBit)
synthAppPrimFun _env pf args =
  throwError . refineError $
    concat
      [ "primitive function",
        " '" <> show pf <> "' ",
        "applied to wrong number of arguments",
        " '" <> show args <> "'"
      ]

-- clears refinement from type
unrefineBaseType :: BaseType -> BaseType
unrefineBaseType = \case
  TypeAtomic _ atomic -> TypeAtomic mempty atomic

-- TODO: take `ty` into account
synthLiteral :: Literal -> BaseType -> Refining BaseType
synthLiteral lit ty = do
  TypeAtomic <$> r <*> return atomic
  where
    atomic = case lit of
      Syn.LiteralInteger _ -> AtomicInt
      Syn.LiteralFloat _ -> AtomicFloat
      Syn.LiteralBit _ -> AtomicBit
      Syn.LiteralChar _ -> AtomicChar
      Syn.LiteralString _ -> AtomicString
    r = reftPreterm $ TermLiteral lit

synthCon :: Env -> F.Symbol -> Refining BaseType
synthCon env x =
  lookupEnv x env
    >>= ( \case
            TypeBaseType ty -> return ty
            TypeFunType funTy ->
              throwError . refineError . concat $
                [ "type synthesis error;",
                  "expected the variable",
                  " '" <> show x <> "' ",
                  "to have be a constant, but it's actually a function of type",
                  " '" <> show funTy <> "'"
                ]
        )

-- | synthFun
--
-- We need to know about `args` since some primitive functions are polymorphic.
synthFun :: Env -> F.Symbol -> [BaseType] -> Refining FunType
-- types should be structurally the same (ignore refinement)
synthFun env x _args = case F.lookupSEnv x env of
  Just (TypeFunType funTy) -> return funTy
  Just _ -> throwError . refineError $ "expected to be a function id: " <> show x
  Nothing -> throwError . refineError $ "unknown function id: " <> show x

-- | Subtype checking (checkSubtype)
--
-- Check that one type is a subtype of another type (taking refinements into
-- account).
checkSubtype :: BaseType -> BaseType -> Refining Cstr
--    forall x : T, p x ==> (p' x')[x' := x]
--  ----------------------------------------------
--    {x : T | p x} <: {x' : T | p' y'}
checkSubtype ty1@(TypeAtomic r1 atom1) (TypeAtomic r2 atom2)
  | atom1 == atom2 =
      return $
        forallCstr
          (reftSymbol r1)
          ty1
          ( headCstr $
              subst
                (reftExpr r2)
                (reftSymbol r2)
                (reftSymbol r1)
          )
checkSubtype ty1 ty2 =
  throwError . refineError $
    "subtyping error; the type"
      <> (" '" <> show ty1 <> "' ")
      <> "cannot be checked to be a subtype of the type"
      <> (" '" <> show ty2 <> "' ")
